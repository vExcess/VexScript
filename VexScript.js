(function ImportVexScript() {
    /*
        Big Thanks To:
            http://craftinginterpreters.com/functions.html
    */

    // Single-character tokens.
    let 
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, ASTERISK,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    OR, BIN_OR,

    // Literals
    IDENTIFIER, STRING, NUMBER,
    
    // Reserved Words
    AND, CLASS, ELSE, FALSE, FUNC, FOR, IF, 
    NULL, VOID, PRINT, RETURN, SUPER, THIS, 
    TRUE, VAR, WHILE,
    
    EOF

    // This is the silliness you have to do when enums don't exist in JS
    ImportVexScript.toString()
        .slice(ImportVexScript.toString().indexOf("let ") + 4, ImportVexScript.toString().indexOf("EOF"))
        .split("\n")
        .map(s => s.trim())
        .filter(s => !s.startsWith("//"))
        .join("")
        .replaceAll("\n", "")
        .replaceAll(" ", "")
        .split(",")
        .filter(s => s.length > 0)
        .forEach((e, i) => eval(`${e} = ${i}`))

    let VexScriptVM = function () {
        this.copyright = 'Copyright <2022> <Vexcess>\n\nPermission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:\n\nThe above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';
    
        this.setErrorHandler(function (type, msg, trace) {
            console.log(type + ": " + msg + "\n" + (trace === undefined ? "" : trace.join("\n")));
        });
        this.setOutputHandler(function (...args) {
            console.log(...args);
        });
    };
    VexScriptVM.prototype.setErrorHandler = function (fxn) {
        this.error = fxn;
    };
    VexScriptVM.prototype.setOutputHandler = function (fxn) {
        this.output = fxn;
    };
    VexScriptVM.prototype.tokenize = function (code) {
        let throwError = this.error;

        let keywords = {};
        Object.assign(keywords, {
            "class": CLASS,
            "else": ELSE,
            "false": FALSE,
            "for": FOR,
            "func": FUNC,
            "if": IF,
            "null": NULL,
            "void": VOID,
            "print": PRINT,
            "return": RETURN,
            "super": SUPER,
            "this": THIS,
            "true": TRUE,
            "var": VAR,
            "while": WHILE
        });
        
        class Token {
            constructor(type, lexeme, literal, line) {
                if (type === undefined) {
                    throwError("SyntaxError", "Unexpected end of input")
                }
                this.type = type;
                this.lexeme = lexeme;
                this.literal = literal = lexeme;
                this.line = line;
            }
            
            toString() {
                return this.type + " " + this.lexeme + " " + this.literal;
            }
        }

        let tokens = [];
        
        function addToken (type, lexeme, literal) {
            tokens.push(new Token(type, lexeme, literal, line));
        }
        
        let start = 0;
        let current = 0;
        let line = 1;
        
        function isDigit (c) {
            return c >= '0' && c <= '9';
        }
        
        function isAtEnd () {
            return current >= code.length;
        }
        
        function peek() {
            if (isAtEnd()) return '\0';
            return code.charAt(current);
        }
        function peekNext() {
            if (current + 1 >= code.length()) return '\0';
            return code.charAt(current + 1);
        } 
        
        function match(expected) {
            if (isAtEnd()) return false;
            if (code.charAt(current) !== expected) return false;
            
            current++;
            return true;
        }
        
        function string() {
            while (peek() !== '"' && !isAtEnd()) {
                if (peek() === '\n') line++;
                current++;
            }
            
            if (isAtEnd()) {
                throwError("SyntaxError", "Unterminated string")
                return;
            }
            
            // The closing ".
            current++;
            
            // Trim the surrounding quotes.
            let value = code.substring(start + 1, current - 1);
            addToken(STRING, value);
        }
        
        function number() {
            while (isDigit(peek())) current++;
            
            // Look for a fractional part.
            if (peek() === '.' && isDigit(peekNext())) {
            // Consume the "."
            current++;
            
            while (isDigit(peek())) advance();
            }
            
            addToken(NUMBER, code.substring(start, current));
        }
        
        
        function isAlpha(c) {
            return (c >= 'a' && c <= 'z') ||
                   (c >= 'A' && c <= 'Z') ||
                   c === '_' || c > "~";
        }
        
        function isAlphaNumeric(c) {
            return isAlpha(c) || isDigit(c);
        }
        
        function identifier() {
            while (isAlphaNumeric(peek())) current++;
            
            let text = code.substring(start, current);
            let type = keywords[text];
            if (type === undefined) type = IDENTIFIER;
            addToken(type, code.substring(start, current));
        }
        
        function scanToken () {
            let c = code.charAt(current++);
            
            switch (c) {
                case '(': addToken(LEFT_PAREN, code.substring(start, current)); break;
                case ')': addToken(RIGHT_PAREN, code.substring(start, current)); break;
                case '{': addToken(LEFT_BRACE, code.substring(start, current)); break;
                case '}': addToken(RIGHT_BRACE, code.substring(start, current)); break;
                case ',': addToken(COMMA, code.substring(start, current)); break;
                case '.': addToken(DOT, code.substring(start, current)); break;
                case '-': addToken(MINUS, code.substring(start, current)); break;
                case '+': addToken(PLUS, code.substring(start, current)); break;
                case ';': addToken(SEMICOLON, code.substring(start, current)); break;
                case '*': addToken(ASTERISK, code.substring(start, current)); break; 

                case '|': addToken(match('|') ? OR : BIN_OR, code.substring(start, current)); break; 
                
                case '!':
                    addToken(match('=') ? BANG_EQUAL : BANG, code.substring(start, current));
                    break;
                case '=':
                    addToken(match('=') ? EQUAL_EQUAL : EQUAL, code.substring(start, current));
                    break;
                case '<':
                    addToken(match('=') ? LESS_EQUAL : LESS, code.substring(start, current));
                    break;
                case '>':
                    addToken(match('=') ? GREATER_EQUAL : GREATER, code.substring(start, current));
                    break;
                    
                case '/':
                    if (match('/')) {
                        // A comment goes until the end of the line.
                        while (peek() !== '\n' && !isAtEnd()) current++;
                    } else {
                    addToken(SLASH, code.substring(start, current));
                    }
                    break;
                    
                case ' ':
                case '\r':
                case '\t':
                    // Ignore whitespace.
                    break;
            
                case '\n':
                    line++;
                    break;
                    
                case '"': string(); break;
                
                default:
                    if (isDigit(c)) {
                        number();
                    } else if (isAlpha(c)) {
                        identifier();
                    } else {
                        throwError("SyntaxError", "Unexpected Token")
                    }
                    break;
            }
        }
        
        while (!isAtEnd()) {
            start = current;
            scanToken();
        }
        
        return tokens;
    };
    VexScriptVM.prototype.createAST = function (tokens) {
        let throwError = this.error;

        let Expr = {
            Binary: (left, operator, right) => ({
                isExpr: true,
                type: "binary",
                left: left,
                operator: operator,
                right: right
            }),
            Logical: (left, operator, right) => ({
                isExpr: true,
                type: "logical",
                left: left,
                operator: operator,
                right: right
            }),
            Grouping: (expression) => ({
                isExpr: true,
                type: "grouping",
                expression: expression
            }),
            Literal: (value) => ({
                isExpr: true,
                type: "literal",
                value: value
            }),
            Unary: (operator, right) => ({
                isExpr: true,
                type: "unary",
                operator: operator,
                right: right
            }),
            Variable: (name, value) => ({
                isExpr: true,
                type: "variable",
                name: name,
                value: value
            }),
            Assign: (name, value) => ({
                isExpr: true,
                type: "assign",
                name: name,
                value: value
            }),
            Call: (callee, paren, args) => ({
                isExpr: true,
                type: "call",
                callee: callee,
                paren: paren,
                args: args
            })
        };

        let Stmt = {
            Expression: (expression) => ({
                type: "expression",
                expression: expression
            }),
            Print: (expression) => ({
                type: "print",
                expression: expression
            }),
            Return: (keyword, value) => ({
                type: "return",
                keyword: keyword,
                value: value
            }),
            Var: (name, initializer) => ({
                type: "var",
                name: name,
                initializer: initializer
            }),
            Block: (statements) => ({
                type: "block",
                statements: statements
            }),
            If: (condition, thenBranch, elseBranch) => ({
                type: "if",
                condition: condition,
                thenBranch: thenBranch,
                elseBranch: elseBranch
            }),
            While: (condition, body) => ({
                type: "while",
                condition: condition,
                body: body
            }),
            Func: (name, params, body) => ({
                type: "function",
                name: name,
                params: params,
                body: body
            })
        };

        class Parser {
            current = 0;

            isAtEnd() {
                return this.current >= tokens.length;
            }
            peek() {
                return tokens[this.current];
            }
            previous() {
                return tokens[this.current - 1];
            }
            advance() {
                if (!this.isAtEnd()) this.current++;
                return this.previous();
            }
            check(type) {
                if (this.isAtEnd()) return false;
                return this.peek().type === type;
            }
            match(...types) {
                for (let type of types) {
                    if (!this.isAtEnd() && tokens[this.current].type === type) {
                        this.current++;
                        return true;
                    }
                }
                return false;
            }

            synchronize() {
                this.advance();

                while (!this.isAtEnd()) {
                    if (this.previous().type === SEMICOLON) return;

                    switch (this.peek().type) {
                        case CLASS:
                        case FUNC:
                        case VAR:
                        case FOR:
                        case IF:
                        case WHILE:
                        case PRINT:
                        case RETURN:
                        return;
                    }

                    this.advance();
                }
            }

            consume(type, message) {
                if (this.check(type)) return this.advance();

                throw throwError("CompilerError", message + "\n" + JSON.stringify(this.peek()));
            } 

            primary() {
                if (!this.isAtEnd()) {
                    switch (tokens[this.current].type) {
                        case FALSE:
                            this.current++;
                            return Expr.Literal(false);
                        case TRUE:
                            this.current++;
                            return Expr.Literal(true);
                        case NULL:
                            this.current++;
                            return Expr.Literal(null);
                        case VOID:
                            this.current++;
                            return Expr.Literal(undefined);
                        case NUMBER:
                            this.current++;
                            return Expr.Literal(Number(this.previous().literal));
                        case STRING:
                            this.current++;
                            return Expr.Literal(this.previous().literal);
                        case LEFT_PAREN:
                            this.current++;
                            let expr = this.expression();
                            this.consume(RIGHT_PAREN, "Expect ')' after expression.");
                            return Expr.Grouping(expr);
                        case IDENTIFIER:
                            this.current++;
                            return Expr.Variable(this.previous().lexeme);
                    }
                }

                throwError("CompilerError", JSON.stringify(this.previous()) + "\nExpected expression.");
            }

            finishCall(callee) {
                let args = [];
                if (!this.check(RIGHT_PAREN)) {
                    do {
                        args.push(this.expression());
                    } while (this.match(COMMA));
                }

                let paren = this.consume(RIGHT_PAREN, "Expect ')' after arguments.");

                if (args.length > 255) {
                    throwError("CompilerError", this.peek() + "\nFunction can't take more than 255 arguments");
                }

                return Expr.Call(callee, paren, args);
            }

            call() {
                let expr = this.primary();

                while (true) { 
                    if (this.match(LEFT_PAREN)) {
                        expr = this.finishCall(expr);
                    } else {
                        break;
                    }
                }

                return expr;
            }
            
            unary() {
                if (this.match(BANG, MINUS)) {
                    let operator = this.previous();
                    let right = this.unary();
                    return Expr.Unary(operator, right);
                }

                return this.call();
            }

            factor() {
                let expr = this.unary();

                while (this.match(SLASH, ASTERISK)) {
                    let operator = this.previous();
                    let right = this.unary();
                    expr = Expr.Binary(expr, operator, right);
                }

                return expr;
            }

            term() {
                let expr = this.factor();

                while (this.match(MINUS, PLUS)) {
                    let operator = this.previous();
                    let right = this.factor();
                    expr = Expr.Binary(expr, operator, right);
                }

                return expr;
            }

            comparison() {
                let expr = this.term();

                while (this.match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
                    let operator = this.previous();
                    let right = this.term();
                    expr = Expr.Binary(expr, operator, right);
                }

                return expr;
            }

            equality() {
                let expr = this.comparison();
                
                while (this.match(BANG_EQUAL, EQUAL_EQUAL)) {
                    let operator = this.previous();
                    let right = this.comparison();
                    expr = Expr.Binary(expr, operator, right);
                }

                return expr;
            }

            and() {
                let expr = this.equality();

                while (this.match(AND)) {
                    let operator = this.previous();
                    let right = this.equality();
                    expr = Expr.Logical(expr, operator, right);
                }

                return expr;
            }

            or() {
                let expr = this.and();

                while (this.match(OR)) {
                    let operator = this.previous();
                    let right = this.and();
                    expr = Expr.Logical(expr, operator, right);
                }

                return expr;
            }

            assignment() {
                let expr = this.or();

                if (this.match(EQUAL)) {
                    let equals = this.previous();
                    let value = this.assignment();

                    if (expr.isExpr) {
                        return Expr.Assign(expr.name, value);
                    }

                    this.error(equals, "Invalid assignment target."); 
                }

                return expr;
            }

            expression() {
                return this.assignment();
            }

            printStatement() {
                let value = this.expression();
                this.consume(SEMICOLON, "Expect ';' after value.");
                return Stmt.Print(value);
            }

            ifStatement() {
                this.consume(LEFT_PAREN, "Expect '(' after 'if'.");
                let condition = this.expression();
                this.consume(RIGHT_PAREN, "Expect ')' after if condition."); 

                let thenBranch = this.statement();
                let elseBranch = null;
                if (this.match(ELSE)) {
                    elseBranch = this.statement();
                }

                return Stmt.If(condition, thenBranch, elseBranch);
            }

            expressionStatement() {
                let expr = this.expression();
                this.consume(SEMICOLON, "Expect ';' after expression.");
                
                return Stmt.Expression(expr);
            }

            block() {
                let statements = [];

                while (!this.check(RIGHT_BRACE) && !this.isAtEnd()) {
                    statements.push(this.declaration());
                }

                this.consume(RIGHT_BRACE, "Expect '}' after block.");
                return statements;
            }

            whileStatement() {
                this.consume(LEFT_PAREN, "Expect '(' after 'while'.");
                let condition = this.expression();
                this.consume(RIGHT_PAREN, "Expect ')' after condition.");
                let body = this.statement();

                return Stmt.While(condition, body);
            }

            forStatement() {
                this.consume(LEFT_PAREN, "Expect '(' after 'for'.");

                let initializer;
                if (this.match(SEMICOLON)) {
                    initializer = null;
                } else if (this.match(VAR)) {
                    initializer = this.varDeclaration();
                } else {
                    initializer = this.expressionStatement();
                }

                let condition = null;
                if (!this.check(SEMICOLON)) {
                    condition = this.expression();
                }
                this.consume(SEMICOLON, "Expect ';' after loop condition.");

                let increment = null;
                if (!this.check(RIGHT_PAREN)) {
                    increment = this.expression();
                }
                this.consume(RIGHT_PAREN, "Expect ')' after for clauses.");

                let body = this.statement();

                if (increment !== null) {
                    body.statements.push(Stmt.Expression(increment));
                }

                if (condition === null) condition = Expr.Literal(true);
                body = Stmt.While(condition, body);

                if (initializer !== null) {
                    body = Stmt.Block([initializer, body]);
                }

                return body;
            }

            returnStatement() {
                let keyword = this.previous();
                let value = null;
                if (!this.check(SEMICOLON)) {
                    value = this.expression();
                }

                this.consume(SEMICOLON, "Expect ';' after return value.");
                return Stmt.Return(keyword, value);
            }

            statement() {
                if (this.match(FOR)) return this.forStatement();
                if (this.match(IF)) return this.ifStatement();
                if (this.match(PRINT)) return this.printStatement();
                if (this.match(RETURN)) return this.returnStatement();
                if (this.match(WHILE)) return this.whileStatement();
                if (this.match(LEFT_BRACE)) return Stmt.Block(this.block());
                return this.expressionStatement();
            }

            varDeclaration() {
                let name = this.consume(IDENTIFIER, "Expect variable name.");

                let initializer = null;
                if (this.match(EQUAL)) {
                    initializer = this.expression();
                }

                this.consume(SEMICOLON, "Expect ';' after variable declaration.");
                return Stmt.Var(name, initializer);
            }

            func(kind) {
                let name = this.consume(IDENTIFIER, "Expect " + kind + " name.");
                this.consume(LEFT_PAREN, "Expect '(' after " + kind + " name.");
                let parameters = [];
                if (!this.check(RIGHT_PAREN)) {
                    do {
                        if (parameters.length > 255) {
                            throwError("SyntaxError", this.peek() + "Can't have more than 255 parameters.");
                        }

                        parameters.push(this.consume(IDENTIFIER, "Expect parameter name."));
                    } while (this.match(COMMA));
                }
                this.consume(RIGHT_PAREN, "Expect ')' after parameters.");

                this.consume(LEFT_BRACE, "Expect '{' before " + kind + " body.");
                let body = this.block();
                return Stmt.Func(name, parameters, body);
            }

            declaration() {
                try {
                    if (this.match(FUNC)) return this.func("function");
                    if (this.match(VAR)) return this.varDeclaration();
                    return this.statement();
                } catch (e) {
                    console.log(e)
                    this.synchronize();
                    return null;
                }
            }

            parse() {
                let statements = [];
                while (this.current < tokens.length) {
                    statements.push(this.declaration());
                }
                return statements;
            }
        }
        
        return new Parser().parse();
    };
    VexScriptVM.prototype.createInterpreter = function () {
        let throwError = this.error;
        let output = this.output;

        class Function_ {
            constructor(declaration, closure) {
                this.declaration = declaration;
                this.closure = closure;
            }

            arity() {
                return this.declaration.params.length;
            }

            call(interpreter, args) {
                let environment = new Environment(this.closure);
                let i;
                for (i = 0; i < this.declaration.params.length; i++) {
                    environment.define(
                        this.declaration.params[i].lexeme,
                        args[i]
                    );
                }
                try {
                    interpreter.executeBlock(this.declaration.body, environment);
                } catch (returnValue) {
                    return returnValue.value;
                }
                return null;
            }

            toString() {
                return "func " + this.declaration.name.lexeme + "() { <native code> }";
            }
        }

        class Environment {
            values = new Map();
            enclosing;

            constructor(enclosing){
                this.enclosing = enclosing ?? null;
            }

            define(name, value) {
                this.values.set(name, value);
            }

            get(name) {
                if (this.values.has(name)) {
                    return this.values.get(name);
                }

                if (this.enclosing !== null) {
                    return this.enclosing.get(name);
                }

                throw throwError("ReferenceError", name + "Undefined variable '" + name.lexeme + "'.");
            }

            assign(name, value) {
                if (this.values.has(name)) {
                    this.values.set(name, value);
                    return;
                }

                if (this.enclosing !== null) {
                    this.enclosing.assign(name, value);
                    return;
                }

                throw throwError("ReferenceError", name + "Undefined variable '" + name.lexeme + "'.");
            }
        }

        class Return {
            constructor(value) {
                this.value = value;
            }
        }

        class Interpreter {
            globals = new Environment();
            environment = this.globals;

            constructor() {
                this.globals.define("millis", {
                    arity: () => 0,
                    call: (interpreter, args) => {
                        return Date.now();
                    },
                    toString: () => { return "() { <native code> }"; }
                });
            }
            
            visitLiteralExpr(expr) {
                return expr.value;
            }

            evaluate(expr) {
                switch (expr.type) {
                    case "binary":
                        return this.visitBinaryExpr(expr);
                    case "logical":
                        return this.visitLogicalExpr(expr);
                    case "unary":
                        return this.visitUnaryExpr(expr);
                    case "grouping":
                        return this.visitGroupingExpr(expr);
                    case "call":
                        return this.visitCallExpr(expr);
                    case "variable":
                        return this.environment.get(expr.name);
                }
                return expr.value;
            }
            
            visitGroupingExpr(expr) {
                return this.evaluate(expr.expression);
            }

            isTruthy(object) {
                if (object === null) return false;
                if (typeof object === "boolean") return object;
                if (object === 0) return false;
                return true;
            }

            isEqual(a, b) {
                if (a === null && b === null) return true;
                if (a === null) return false;
                return a === b;
            }

            visitUnaryExpr(expr) {
                let right = this.evaluate(expr.right);

                switch (expr.operator.type) {
                    case BANG:
                        return !this.isTruthy(right);
                    case MINUS:
                        return -Number(right);
                }

                return null; // Unreachable.
            }

            checkNumberOperand(operator, operand) {
                if (typeof operand === "number") return;
                throwError("TypeError", JSON.stringify(operator) + "\nOperand must be a number.");
            }

            checkNumberOperands(operator, left, right) {
                if (typeof left === "number" && typeof right === "number") return;
                throwError("TypeError", JSON.stringify(operator) + "\nOperands must be a number.");
            }

            visitBinaryExpr(expr) {
                let left = this.evaluate(expr.left);
                let right = this.evaluate(expr.right); 

                switch (expr.operator.type) {
                    case GREATER:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left > right;
                    case GREATER_EQUAL:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left >= right;
                    case LESS:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left < right;
                    case LESS_EQUAL:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left <= right;
                    case BANG_EQUAL: 
                        return !this.isEqual(left, right);
                    case EQUAL_EQUAL: 
                        return this.isEqual(left, right);

                    case MINUS:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left - right;
                    case SLASH:
                        this.checkNumberOperands(expr.operator, left, right);
                        if (left === 0 && right === 0) {
                            return undefined;
                        }
                        return left / right;
                    case ASTERISK:
                        this.checkNumberOperands(expr.operator, left, right);
                        return left * right;
                    case PLUS:
                        if (typeof left === "number" && typeof right === "number") {
                            return left + right;
                        } 

                        if (typeof left === "string" && typeof right === "string") {
                            return left + right;
                        }

                        if (typeof left === "number" && typeof right === "string") {
                            return left.toString() + right;
                        }

                        if (typeof left === "string" && typeof right === "number") {
                            return left + right.toString();
                        }

                        throwError("TypeError", JSON.stringify(expr.operator) + "\nOperands to '+' operator must be numbers or strings");
                }

                // Unreachable.
                return null;
            }

            stringify(object) {
                if (object === null) return "null";
                if (object === undefined) return "void";

                if (typeof object === "number") {
                    let text = object.toString();
                    if (text.endsWith(".0")) {
                        text = text.substring(0, text.length() - 2);
                    }
                    return text;
                }

                return object.toString();
            }

            visitVarStmt(stmt) {
                let value = null;
                if (stmt.initializer !== null) {
                    value = this.evaluate(stmt.initializer);
                }

                this.environment.define(stmt.name.lexeme, value);
                return null;
            }

            visitVariableExpr(expr) {
                return this.environment.get(expr.name);
            }

            visitPrintStmt(stmt) {
                let value = this.evaluate(stmt.expression);
                if (value instanceof Function_) {
                    output(value.toString());
                } else {
                    output(this.stringify(value));
                }                
                return null;
            }

            visitAssignExpr(expr) {
                if (expr.type === "call") {
                    return this.visitCallExpr(expr);
                } else {
                    let value = this.evaluate(expr.value);
                    this.environment.assign(expr.name, value);
                    return value;
                }
            }

            visitIfStmt(stmt) {
                if (this.isTruthy(this.evaluate(stmt.condition))) {
                    this.execute(stmt.thenBranch);
                } else if (stmt.elseBranch !== null) {
                    this.execute(stmt.elseBranch);
                }
                return null;
            }

            visitLogicalExpr(expr) {
                let left = this.evaluate(expr.left);
                
                if (expr.operator.type === OR) {
                    if (this.isTruthy(left)) return left;
                } else {
                    if (!this.isTruthy(left)) return left;
                }

                return this.evaluate(expr.right);
            }

            visitWhileStmt(stmt) {
                while (this.isTruthy(this.evaluate(stmt.condition))) {
                    this.execute(stmt.body);
                }
                return null;
            }

            executeBlock(statements, environment) {
                let previous = this.environment;
                try {
                    this.environment = environment;

                    for (let statement in statements) {
                        this.execute(statements[statement]);
                    }
                } finally {
                    this.environment = previous;
                }
            }

            visitBlockStmt(stmt) {
                this.executeBlock(stmt.statements, new Environment(this.environment));
                return null;
            }

            visitCallExpr(expr) {
                let callee = this.evaluate(expr.callee);

                let args = [];
                for (let arg of expr.args) { 
                    args.push(this.evaluate(arg));
                }

                let fxn = callee;

                while (args.length < fxn.arity()) {
                    args.push(undefined);
                }

                if (!fxn.call) {
                    throwError("SyntaxError", JSON.stringify(fxn) + "\nCan only call functions and classes");
                }

                return fxn.call(this, args);
            }

            visitFunctionStmt(stmt) {
                let fxn = new Function_(stmt, this.environment);
                this.environment.define(stmt.name.lexeme, fxn);
                return null;
            }

            visitReturnStmt(stmt) {
                let value = null;
                if (stmt.value !== null) value = this.evaluate(stmt.value);

                throw new Return(value);
            }

            execute(stmt) {
                switch (stmt.type) {
                    case "print":
                        return this.visitPrintStmt(stmt);
                    case "var":
                        return this.visitVarStmt(stmt);
                    case "expression":
                        return this.visitAssignExpr(stmt.expression);
                    case "block":
                        return this.visitBlockStmt(stmt);
                    case "if":
                        return this.visitIfStmt(stmt);
                    case "while":
                        return this.visitWhileStmt(stmt);
                    case "function":
                        return this.visitFunctionStmt(stmt);
                    case "return":
                        return this.visitReturnStmt(stmt);
                }
            }

            interpret(statements) { 
                // try {
                    let i;
                    for (i = 0; i < statements.length; i++) {
                        this.execute(statements[i]);
                    }
                // } catch (e) {
                //     throwError("VMError", e);
                // }
            }
        }

        return new Interpreter();
    };
    VexScriptVM.prototype.run = function (code) {
        this.tokens = this.tokenize(code);
        // console.log(this.tokens)
        this.program = this.createAST(this.tokens);
        // console.log(JSON.stringify(this.program, "", "    "))
        this.interpreter = this.createInterpreter();
        this.interpreter.interpret(this.program);
    };

    window.VexScriptVM = VexScriptVM;
})()
