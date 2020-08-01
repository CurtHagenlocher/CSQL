section CSQL;

[DataSource.Kind="CSQL", Publish="CSQL.UI"]
shared CSQL.Tables = (server as text, database as text) as table =>
let
    source = AdoDotNet.DataSource("System.Data.SqlClient", [server=server, database=database]),
    tables = Value.NativeQuery(source, "select SCHEMA_NAME(schema_id) as [Schema], OBJECT_NAME(object_id) as [Table] from sys.tables order by [Schema], [Table]"),
    withName = Table.AddColumn(tables, "Name", each [Schema] & "." & [Table]),
    withData = Table.AddColumn(withName, "Data", each CSQL.Table(source, server, database, [Schema], [Table])),
    asNavigation = Table.ToNavigationTable(withData, {"Schema", "Table"}, "Name", "Data")
in
    asNavigation;

CSQL.Table = (source, server as text, database as text, schema as text, table as text) as table =>
let
    tableName = QuoteIdentifier(schema) & "." & QuoteIdentifier(table),
    columnNames = (params) => if params[columns]? = null then "*" else Text.Combine(List.Transform(params[columns], QuoteIdentifier), ","),
    top = (params) => if params[count]? = null then " " else " top " & Text.From(params[count]) & " ",
    predicates = (params) => if params[filter]? = null then "" else " where " & params[filter][Text],
    query = (params) => "select" & top(params) & columnNames(params) & " from " & tableName & predicates(params),
    execute = (params) => Value.NativeQuery(source, Diagnostics.Trace(1, query(params), query(params))),
    tableType = Value.Type(execute([count=0])),
    view = (params) => Table.View(null, [
        GetType = () => if params[columns]? = null then tableType
            else Value.Type(Table.SelectColumns(#table(tableType, {}), params[columns])),
        GetRows = () => execute(params),
        GetExpression = () => Invocation(Value.NativeQuery, {
            Invocation(CSQL.Table, {Constant(server), Constant(database), Constant(schema), Constant(table)}),
            Constant(query(params))
        }),
        
        OnSelectColumns = (columns) => @view(params & [columns = columns]),
        OnTake = (count) => if count < Record.FieldOrDefault(params, "count", 1e12) then @view(params & [count=count]) else @view(params),
        OnSelectRows = (condition) => if (params[count]? <> null) then ... else
            @view(Record.AddField(params, "filter", AddFilter(tableType, condition, params[filter]?)))
    ])
in
    view([]);

Constant = (value) => [Kind="Constant", Value=value];
Invocation = (fn as function, args as list) => [Kind="Invocation", Function=Constant(fn), Arguments=args];

QuoteIdentifier = (identifier) => "[" & Text.Replace(identifier, "]", "]]") & "]";

Table.ToNavigationTable = (
    table as table,
    keyColumns as list,
    nameColumn as text,
    dataColumn as text
) as table =>
    let
        tableType = Value.Type(table),
        newTableType = Type.AddTableKey(tableType, keyColumns, true) meta 
        [
            NavigationTable.NameColumn = nameColumn, 
            NavigationTable.DataColumn = dataColumn,
            Preview.DelayColumn = dataColumn
        ],
        navigationTable = Value.ReplaceType(table, newTableType)
    in
        navigationTable;

CSQL = [
    TestConnection = (dataSourcePath) => 
        let
            json = Json.Document(dataSourcePath),
            server = json[server],
            database = json[database]
        in
            { "CSQL.Tables", server, database }, 
    Authentication = [
        Windows = [],
        UsernamePassword = []
    ],
    Label = "ADO.NET Query Folding for SQL",
    SupportsEncryption = false
];

Pattern.Value = (optional condition, optional selector) => (value) =>
    if (condition = null or condition(value)) then
        if (selector <> null) then selector(value)
        else value
    else null;

Pattern.Choice = (keySelector, options) => (value) => let
    pattern = Record.Field(options, keySelector(value)),
    appliedPattern = pattern(value)
in
    appliedPattern;

Pattern.Record = (pattern, optional transform) => (value) => let
    fieldNames = Record.FieldNames(pattern),
    captured = List.Transform(fieldNames, each let
        patternFieldValue = Record.Field(pattern, _),
        valueFieldValue = Record.Field(value, _),
        capturedFieldValue =
            if (patternFieldValue is function) then patternFieldValue(valueFieldValue)
            else if (patternFieldValue = valueFieldValue) then valueFieldValue
            else ...
    in
        [Name = _, Value = capturedFieldValue]),
    matchedRecord = Record.FromTable(Table.FromRecords(captured)),
    transformedRecord = if (transform <> null) then transform(matchedRecord) else matchedRecord
in
    transformedRecord;

AddFilter = (tableType, predicate, previous) =>
    let
        anything = Pattern.Value(),
        return = (value) => Pattern.Value(null, each value),

        typeOracle = (column) => Type.TableColumn(tableType, column),

        sqlLogicalType = Value.Type(() => null), // sentinel type
        makeLogical = (expr) => if expr[Type] = sqlLogicalType then expr
            else if Type.Is(expr[Type], type nullable logical) then createBinaryExpr(operatorExpr("Equals"), expr, createConstantExpr(true))
            else ...,

        filterExpr = Pattern.Choice(
            each [Kind],
            [
                Binary = binaryExpr,
                FieldAccess = fieldAccessExpr,
                Constant = constantExpr
            ]),

        fieldAccessExpr = Pattern.Record(
            [
                Kind = "FieldAccess",
                Expression = anything,
                MemberName = anything
            ],
            each if [Expression] <> RowExpression.Row then ... else
                [
                    Type = typeOracle([MemberName]),
                    Text = QuoteIdentifier([MemberName]),
                    Precedence = -1
                ]),

        constantExpr = Pattern.Record(
            [
                Kind = "Constant",
                Value = anything
            ],
            each createConstantExpr([Value])),

        binaryExpr = Pattern.Record(
            [
                Kind = "Binary",
                Operator = operatorExpr,
                Left = filterExpr,
                Right = filterExpr
            ],
            each createBinaryExpr([Operator], [Left], [Right])),
            
        operatorExpr = Pattern.Choice(
            each _,
            [
                /* Not supported:
                Multiply = return([Text = "*", Precedence = 2]),
                Divide = return([Text = "/", Precedence = 2]),
                Add = return([Text = "+", Precedence = 3]),
                Subtract = return([Text = "-", Precedence = 3]),
                Concatenate = return([Text = "+", Precedence = 3]),
                */
                Equals = return([Text = "=", Precedence = 4, LogicalOutput = true]),
                NotEquals = return([Text = "<>", Precedence = 4, LogicalOutput = true]),
                GreaterThan = return([Text = ">", Precedence = 4, LogicalOutput = true]),
                GreaterThanOrEquals = return([Text = ">=", Precedence = 4, LogicalOutput = true]),
                LessThan = return([Text = "<", Precedence = 4, LogicalOutput = true]),
                LessThanOrEquals = return([Text = "<=", Precedence = 4, LogicalOutput = true]),
                Not = return([Text = "not", Precedence = 5, LogicalInput = true]),
                And = return([Text = "and", Precedence = 6, LogicalInput = true]),
                Or = return([Text = "or", Precedence = 7, LogicalInput = true])
            ]),

        createConstantExpr = (value) =>
            [
                Type = Value.Type(value),
                Text = escapeValue(value),
                Precedence = -1
            ],

        createBinaryExpr = (op, left, right) =>
            let
                logicalInput = (op[LogicalInput]? = true),
                logicalOutput = (op[LogicalOutput]? = true),
                l = if logicalInput then makeLogical(left) else left,
                r = if logicalInput then makeLogical(right) else right,
                leftText = if (comparePrecedence(op, l) < 0) then "(" & l[Text] & ")" else l[Text],
                rightText = if (comparePrecedence(op, r) < 0) then "(" & r[Text] & ")" else r[Text]
            in
                if not logicalInput and (l[Type] = sqlLogicalType or r[Type] = sqlLogicalType) then ...
                else if l[Type] = type null then
                    if r[Type] <> type null then @createBinaryExpr(op, r, l) else ...
                else if op = "=" and r[Type] = type null then
                    [Type = sqlLogicalType, Text = "(" & l[Text] & ") is null"]
                else if op = "<>" and r[Type] = type null then
                    [Type = sqlLogicalType, Text = "(" & l[Text] & ") is not null"]
                else if (Type.NonNullable(l[Type]) = Type.NonNullable(r[Type]))
                    and (not Type.IsNullable(l[Type]) or not Type.IsNullable(r[Type])) then
                        [
                            Type = if logicalOutput then sqlLogicalType else Type.NonNullable(l[Type]),
                            Text = Text.Combine({leftText, " ", op[Text], " ", rightText}),
                            Precedence = precedence(op)
                        ]
                else ...,

        precedence = (expressionOrOperator) =>
            Record.FieldOrDefault(expressionOrOperator, "Precedence", 1000),

        comparePrecedence = (x, y) =>
            if (precedence(x) < precedence(y)) then -1
            else if (precedence(x) > precedence(y)) then 1
            else 0,

        escapeValue = (value) =>
            if (value = null) then "null"
            else if (value is date) then "'" & Date.ToText(value, "yyyy-MM-dd") & "'"
            else if (value is datetime) then "'" & DateTime.ToText(value, "yyyy-MM-dd HH:mm:ss") & "'"
            else if (value is datetimezone) then "'" & DateTimeZone.ToText(value, "yyyy-MM-dd HH:mm:ss zzzz") & "'"
            else if (value is text) then "N'" & Text.Replace(value, "'", "''") & "'"
            else if (value is logical) then if value then "1" else "0"
            else ...,

        filter = filterExpr(RowExpression.From(predicate))
    in
        if previous <> null then createBinaryExpr(operatorExpr("And"), previous, filter) else makeLogical(filter);

CSQL.UI = [
    Category = "Database",
    ButtonText = { "ADO.NET Query Folding for SQL", "Query Folding via ADO.NET sample for SQL Server" }
];
