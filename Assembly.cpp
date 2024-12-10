#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <sstream>
#include <stdexcept>
using namespace std;

// Token Types Enumeration
enum TokenType
{
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULO,
    ASSIGN,
    EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    GREATER_THAN,
    LESS_EQUAL,
    GREATER_EQUAL,
    IF,
    ELSE,
    WHILE,
    FOR,
    RETURN,
    INT,
    FLOAT,
    DOUBLE,
    CHAR,
    STRING,
    VOID,
    IDENTIFIER,
    INTEGER_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    SEMICOLON,
    COMMA,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LOGICAL_AND,
    LOGICAL_OR,
    LOGICAL_NOT,
    END_OF_FILE,
    UNKNOWN
};

// Token Structure
struct Token
{
    TokenType type;
    string value;
    int line, column;

    string toString() const
    {
        static const unordered_map<TokenType, string> typeNames = {
            {PLUS, "PLUS"}, {MINUS, "MINUS"}, {MULTIPLY, "MULTIPLY"}, {DIVIDE, "DIVIDE"}, {MODULO, "MODULO"}, {ASSIGN, "ASSIGN"}, {EQUAL, "EQUAL"}, {NOT_EQUAL, "NOT_EQUAL"}, {LESS_THAN, "LESS_THAN"}, {GREATER_THAN, "GREATER_THAN"}, {LESS_EQUAL, "LESS_EQUAL"}, {GREATER_EQUAL, "GREATER_EQUAL"}, {IF, "IF"}, {ELSE, "ELSE"}, {WHILE, "WHILE"}, {FOR, "FOR"}, {RETURN, "RETURN"}, {INT, "INT"}, {FLOAT, "FLOAT"}, {DOUBLE, "DOUBLE"}, {CHAR, "CHAR"}, {STRING, "STRING"}, {VOID, "VOID"}, {IDENTIFIER, "IDENTIFIER"}, {INTEGER_LITERAL, "INTEGER_LITERAL"}, {FLOAT_LITERAL, "FLOAT_LITERAL"}, {STRING_LITERAL, "STRING_LITERAL"}, {SEMICOLON, "SEMICOLON"}, {COMMA, "COMMA"}, {LEFT_PAREN, "LEFT_PAREN"}, {RIGHT_PAREN, "RIGHT_PAREN"}, {LEFT_BRACE, "LEFT_BRACE"}, {RIGHT_BRACE, "RIGHT_BRACE"}, {LOGICAL_AND, "LOGICAL_AND"}, {LOGICAL_OR, "LOGICAL_OR"}, {LOGICAL_NOT, "LOGICAL_NOT"}, {END_OF_FILE, "END_OF_FILE"}, {UNKNOWN, "UNKNOWN"}};

        auto it = typeNames.find(type);
        string typeName = it != typeNames.end() ? it->second : "UNKNOWN";
        return "Token{type: " + typeName + ", value: '" + value + "', line: " + to_string(line) + ", column: " + to_string(column) + "}";
    }
};

// Symbol Table Class
class SymbolTable
{
private:
    struct Symbol
    {
        string identifier;
        string type;
        string scope;
        int line;

        string toString() const
        {
            return "Identifier: " + identifier + ", Type: " + type + ", Scope: " + scope + ", Declared at line: " + to_string(line);
        }
    };

    vector<Symbol> symbols;

public:
    void addEntry(const string &identifier, const string &type, const string &scope, int line)
    {
        symbols.push_back({identifier, type, scope, line});
    }

    void print() const
    {
        cout << "\nSymbol Table:\n";
        for (const auto &symbol : symbols)
        {
            cout << symbol.toString() << endl;
        }
    }
};

// Lexer Class
class Lexer
{
private:
    string source;
    size_t current;
    int line, column;
    vector<Token> tokens;

public:
    SymbolTable symbolTable;
    string currentScope = "global";

    Lexer(const string &src) : source(src), current(0), line(1), column(1) {}

    vector<Token> tokenize()
    {
        tokens.clear();
        while (!isAtEnd())
        {
            char c = advance();

            if (isspace(c))
            {
                if (c == '\n')
                {
                    line++;
                    column = 1;
                }
                continue;
            }

            switch (c)
            {
            case '+':
                tokens.push_back(createToken(PLUS, "+"));
                break;
            case '-':
                tokens.push_back(createToken(MINUS, "-"));
                break;
            case '*':
                tokens.push_back(createToken(MULTIPLY, "*"));
                break;
            case '/':
                tokens.push_back(createToken(DIVIDE, "/"));
                break;
            case '%':
                tokens.push_back(createToken(MODULO, "%"));
                break;
            case '=':
                tokens.push_back(match('=') ? createToken(EQUAL, "==") : createToken(ASSIGN, "="));
                break;
            case '<':
                tokens.push_back(match('=') ? createToken(LESS_EQUAL, "<=") : createToken(LESS_THAN, "<"));
                break;
            case '>':
                tokens.push_back(match('=') ? createToken(GREATER_EQUAL, ">=") : createToken(GREATER_THAN, ">"));
                break;
            case '!':
                tokens.push_back(match('=') ? createToken(NOT_EQUAL, "!=") : createToken(LOGICAL_NOT, "!"));
                break;
            case '(':
                tokens.push_back(createToken(LEFT_PAREN, "("));
                break;
            case ')':
                tokens.push_back(createToken(RIGHT_PAREN, ")"));
                break;
            case '{':
                tokens.push_back(createToken(LEFT_BRACE, "{"));
                break;
            case '}':
                tokens.push_back(createToken(RIGHT_BRACE, "}"));
                break;
            case ';':
                tokens.push_back(createToken(SEMICOLON, ";"));
                break;
            case ',':
                tokens.push_back(createToken(COMMA, ","));
                break;
            case '"':
                tokens.push_back(tokenizeStringLiteral());
                break;

            default:
                if (isdigit(c))
                {
                    tokens.push_back(tokenizeNumber(c));
                }
                else if (isalpha(c) || c == '_')
                {
                    tokens.push_back(tokenizeIdentifierOrKeyword(c));
                }
                else
                {
                    tokens.push_back(createToken(UNKNOWN, string(1, c)));
                }
            }
        }

        tokens.push_back({END_OF_FILE, "", line, column});
        return tokens;
    }

private:
    bool isAtEnd() const
    {
        return current >= source.length();
    }

    char advance()
    {
        column++;
        return source[current++];
    }

    bool match(char expected)
    {
        if (isAtEnd() || source[current] != expected)
            return false;
        current++;
        column++;
        return true;
    }

    Token createToken(TokenType type, const string &value = "")
    {
        return {type, value, line, column};
    }

    Token tokenizeNumber(char first)
    {
        string number(1, first);
        while (!isAtEnd() && isdigit(source[current]))
        {
            number += advance();
        }

        if (!isAtEnd() && source[current] == '.')
        {
            number += advance();
            while (!isAtEnd() && isdigit(source[current]))
            {
                number += advance();
            }
            return {FLOAT_LITERAL, number, line, column};
        }

        return {INTEGER_LITERAL, number, line, column};
    }

    Token tokenizeIdentifierOrKeyword(char first)
    {
        string identifier(1, first);
        while (!isAtEnd() && (isalnum(source[current]) || source[current] == '_'))
        {
            identifier += advance();
        }

        static const unordered_map<string, TokenType> keywords = {
            {"if", IF}, {"else", ELSE}, {"while", WHILE}, {"for", FOR}, {"return", RETURN}, {"int", INT}, {"float", FLOAT}, {"double", DOUBLE}, {"char", CHAR}, {"string", STRING}, {"void", VOID}};

        auto it = keywords.find(identifier);
        if (it != keywords.end())
        {
            return {it->second, identifier, line, column};
        }
        else
        {
            if (!tokens.empty() && (tokens.back().type == INT || tokens.back().type == FLOAT ||
                                    tokens.back().type == DOUBLE || tokens.back().type == CHAR ||
                                    tokens.back().type == STRING || tokens.back().type == VOID))
            {
                string type = tokens.back().value;
                symbolTable.addEntry(identifier, type, currentScope, line);
            }
            return {IDENTIFIER, identifier, line, column};
        }
    }

    Token tokenizeStringLiteral()
    {
        string literal;
        while (!isAtEnd() && source[current] != '"')
        {
            literal += advance();
        }
        if (!isAtEnd())
            advance(); // Consume closing quote
        return {STRING_LITERAL, literal, line, column};
    }
};

// Intermediate Code Generator
class IntermediateCodeGenerator
{
public:
    struct ThreeAddressCode
    {
        string op;
        string arg1;
        string arg2;
        string result;

        string toString() const
        {
            if (arg2.empty())
            {
                return result + " = " + arg1;
            }
            return result + " = " + arg1 + " " + op + " " + arg2;
        }
    };

    vector<ThreeAddressCode> generate(const vector<Token> &tokens)
    {
        vector<ThreeAddressCode> intermediateCode;

        // Simple intermediate code generation based on tokens
        for (size_t i = 0; i < tokens.size(); ++i)
        {
            if (tokens[i].type == IDENTIFIER &&
                i + 1 < tokens.size() && tokens[i + 1].type == ASSIGN)
            {

                // Assignment handling
                if (i + 2 < tokens.size())
                {
                    intermediateCode.push_back({"=",
                                                tokens[i + 2].value,
                                                "",
                                                tokens[i].value});
                }
            }

            // Basic arithmetic operation handling
            if (tokens[i].type == PLUS || tokens[i].type == MINUS ||
                tokens[i].type == MULTIPLY || tokens[i].type == DIVIDE)
            {

                if (i > 0 && i + 1 < tokens.size())
                {
                    intermediateCode.push_back({tokenTypeToString(tokens[i].type),
                                                tokens[i - 1].value,
                                                tokens[i + 1].value,
                                                "temp" + to_string(intermediateCode.size())});
                }
            }
        }

        return intermediateCode;
    }

private:
    string tokenTypeToString(TokenType type)
    {
        switch (type)
        {
        case PLUS:
            return "+";
        case MINUS:
            return "-";
        case MULTIPLY:
            return "*";
        case DIVIDE:
            return "/";
        default:
            return "?";
        }
    }
};

// Assembly Code Generator
class AssemblyGenerator
{
public:
    string generate(const vector<IntermediateCodeGenerator::ThreeAddressCode> &intermediateCode)
    {
        stringstream assembly;

        assembly << ".intel_syntax noprefix\n";
        assembly << ".global main\n\n";
        assembly << "main:\n";
        assembly << "    push rbp\n";
        assembly << "    mov rbp, rsp\n\n";

        for (const auto &code : intermediateCode)
        {
            generateInstruction(code, assembly);
        }

        assembly << "    mov rax, 0\n";
        assembly << "    leave\n";
        assembly << "    ret\n";

        return assembly.str();
    }

private:
    void generateInstruction(
        const IntermediateCodeGenerator::ThreeAddressCode &code,
        stringstream &assembly)
    {
        if (code.op == "=")
        {
            assembly << "    # Assignment\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    mov [" << code.result << "], rax\n";
        }
        else if (code.op == "+")
        {
            assembly << "    # Addition\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    add rax, " << code.arg2 << "\n";
            assembly << "    mov [" << code.result << "], rax\n";
        }
        else if (code.op == "-")
        {
            assembly << "    # Subtraction\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    sub rax, " << code.arg2 << "\n";
            assembly << "    mov [" << code.result << "], rax\n";
        }
        else if (code.op == "*")
        {
            assembly << "    # Multiplication\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    imul rax, " << code.arg2 << "\n";
            assembly << "    mov [" << code.result << "], rax\n";
        }
        else if (code.op == "/")
        {
            assembly << "    # Division\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    mov rbx, " << code.arg2 << "\n";
            assembly << "    div rbx\n";
            assembly << "    mov [" << code.result << "], rax\n";
        }
    }
};

// Compiler Class
class Compiler
{
public:
    void compile(const string &sourceCode)
    {
        Lexer lexer(sourceCode);
        vector<Token> tokens = lexer.tokenize();

        // Print tokens
        cout << "Tokens:\n";
        for (const auto &token : tokens)
        {
            cout << token.toString() << endl;
        }

        // Print Symbol Table
        lexer.symbolTable.print();

        // Intermediate Code Generation
        IntermediateCodeGenerator intermediateGenerator;
        auto intermediateCode = intermediateGenerator.generate(tokens);

        // Print Intermediate Code
        cout << "\nIntermediate Code:\n";
        for (const auto &code : intermediateCode)
        {
            cout << code.toString() << endl;
        }

        // Assembly Code Generation
        AssemblyGenerator assemblyGenerator;
        string assemblyCode = assemblyGenerator.generate(intermediateCode);

        // Print Assembly Code
        cout << "\nAssembly Code:\n";
        cout << assemblyCode << endl;
    }
};

int main()
{
    string sourceCode = R"(
        // Variable declarations with different types
        int x = 10;
        float pi = 3.14159;
        double precision = 3.14159265358979;
        string greeting = "Hello, World!";
        
        // Conditional statements
        if (x > 5) {
            int temp = x * 2;
            float result = temp + pi;
        } else {
            int alternate = x + 5;
        }
        
        // Loop constructs
        for (int i = 0; i < 10; i = i + 1) {
            int squared = i * i;
            float fraction = squared / 2.0;
        }
        
        while (x < 20) {
            x = x + 2;
            if (x % 2 == 0) {
                int evenCount = x / 2;
            }
        }
        
        // Complex arithmetic and logical operations
        int a = 5;
        int b = 3;
        int c = a + b * 2;
        
        // Logical operations
        bool isTrue = (a > b) && (c < 20);
        bool isFalse = !isTrue || (a == b);
        
        // Function-like structure (simplified)
        int calculate(int param1, float param2) {
            return param1 * param2;
        }
        
        // Return statement
        return 0;
    )";

    Compiler compiler;
    compiler.compile(sourceCode);

    return 0;
}