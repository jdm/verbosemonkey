#include <iostream>
#include <string>
#include <cctype>
#include <list>
#include <limits>
#include <cstring>
#include <cstdlib>
#include <vector>
#include <map>

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))

struct Token
{
  enum TokenType {
    IDENTIFIER,
    NUMBER,
    STRING,
    FUNCTION,
    SUB,
    DIM,
    IF,
    ELSEIF,
    ELSE,
    END,
    OP,
    COMMA,
    LPAREN,
    RPAREN,
    PROPERTY_ACCESS,
    EOL
  } type;

  std::string value;
};

struct Keyword {
  const char *value;
  Token::TokenType type;
} keywords[] = {
  { "dim", Token::DIM },
  { "if", Token::IF },
  { "else", Token::ELSE },
  { "elseif", Token::ELSEIF },
  { "end", Token::END },
  { "sub", Token::SUB },
  { "function", Token::FUNCTION }
};

struct SingleToken {
  char c;
  Token::TokenType type;
} singles[] = {
  { '(', Token::LPAREN },
  { ')', Token::RPAREN },
  { '+', Token::OP },
  { '-', Token::OP },
  { '*', Token::OP },
  { '/', Token::OP },
  { '%', Token::OP },
  { '.', Token::PROPERTY_ACCESS },
  { ',', Token::COMMA }//,
  //  { '\n', Token::EOL }
};

bool isValidIdentifier(char c)
{
  return std::isalnum(c) || c == '_' || c == '-';
}

bool isValidDigit(char c)
{
  //TODO: non-hack for floating point (0.....0, 0.0.0.0.0)
  return std::isdigit(c) || c == '.';
}

bool isPartOfString(char c)
{
  return c != '"' && c != '\'';
}

bool getInputWhile(bool (*predicate)(char c), std::string &buf)
{
  char c;
  bool moreInput = true;
  while(1)
  {
    c = std::cin.get();
    if(std::cin.eof())
    {
      moreInput = false;
      break;
    }
    
    if(predicate(c))
      buf += c;
    else
      break;
  }

  if(moreInput)
    std::cin.putback(c);
  return moreInput;
}

Token *getToken()
{
  bool error = false;
  Token *tok = new Token;
  char c = ' ';
  //FIXME: make EOL a real token?
  while(std::cin.good() && (c == ' ' || c == '\t' || c == '\n'))
    c = std::cin.get();

  if(std::cin.eof())
  {
    error = true;
    c = 0;
  }
  else tok->value = c;

  if(std::isalpha(c))
  {
    getInputWhile(isValidIdentifier, tok->value);
    tok->type = Token::IDENTIFIER;
    
    for(unsigned int i = 0; i < ARRAY_SIZE(keywords); i++)
      if(keywords[i].value == tok->value)
      {
        tok->type = keywords[i].type;
        break;
      }
  }
  else if(std::isdigit(c))
  {
    tok->type = Token::NUMBER;
    getInputWhile(isValidDigit, tok->value);
  }
  else if(c == '"' || c == '\'')
  {
    tok->type = Token::STRING;
    do {
      getInputWhile(isPartOfString, tok->value);
    } while(std::cin.peek() != c);
    std::cin.get();
    tok->value = tok->value.substr(1, tok->value.size() - 1);
  }
  else
  { 
    unsigned int i;
    for(i = 0; i < ARRAY_SIZE(singles); i++)
      if(singles[i].c == c)
      {
        tok->type = singles[i].type;
        break;
      }
    if(i == ARRAY_SIZE(singles))
      error = true;
  }

  if(error)
  {
    if(tok->value.size())
    {
      std::cerr << "Error: unexpected input (" << tok->value << ")" << std::endl;
      std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
    delete tok;
    tok = NULL;
  }
  return tok;
}

namespace ast
{
  class Node
  {
   public:
    virtual ~Node() {}
    virtual std::string codeGen() { return ""; }
    virtual std::string toString() = 0;
  };
  
  class Expr : public Node
  {
   public:
    virtual ~Expr() {}
    virtual std::string toString() { return "????"; };
  };

  class Literal : public Expr
  {
   public:
    Literal(int val) : type_(INT) { value_.ival = val; }
    Literal(double val) : type_(DOUBLE) { value_.dval = val; }
    Literal(const std::string &val)
    {
      if(std::strchr(val.c_str(), '.'))
        Literal(std::strtod(val.c_str(), NULL));
      else Literal(std::atoi(val.c_str()));
    }
    std::string toString() { return "LITERAL"; }
    
   private:
    enum {
      INT,
      DOUBLE
    } type_;
    union {
      int ival;
      double dval;
    } value_;
  };

  class Identifier : public Expr
  {
   public:
    Identifier(const std::string &name) : name_(name) {}
    std::string toString() { return "IDENTIFIER"; }

   private:
    std::string name_;
  };

  class Dim : public Node
  {
   public:
    Dim(const std::string &name) : name_(name) {}
    std::string toString() { return "DIM"; }

   private:
    std::string name_;
  };

  class BinaryExpr : public Expr
  {
   public:
    BinaryExpr(char op, Expr *lhs, Expr *rhs)
    : op_(op), lhs_(lhs), rhs_(rhs) {}
    std::string toString() { return lhs_->toString() + " OP " + rhs_->toString(); }

   private:
    char op_;
    Expr *lhs_, *rhs_;
  };

  class CallExpr : public Expr
  {
   public:
    CallExpr(const std::string &callee, const std::vector<Expr *> &args)
    : callee_(callee), args_(args) {}
    std::string toString() { return "CALL"; }

   private:
    std::string callee_;
    std::vector<Expr *> args_;
  };

  class Function : public Node
  {
   public:
    Function(const std::string &name, const std::vector<std::string> &args, Expr *body)
    : name_(name), args_(args), body_(body) {}
    std::string toString() { return std::string("FUNCTION\n") + "  " + body_->toString(); }

   private:
    std::string name_;
    std::vector<std::string> args_;
    Expr *body_;
  };
}

typedef std::list<Token *> TokenList;
typedef TokenList::const_reverse_iterator TokenIt;

Token *nextToken(TokenIt &it, const TokenIt &end)
{
  it++;
  if(it != end)
    return *it;
  else return NULL;
}

ast::Node *errorN(const char *str)
{
  std::cerr << str << std::endl;
  return NULL;
}

ast::Expr *errorE(const char *str)
{
  errorN(str);
  return NULL;
}

int opPrecedence(char op)
{
  static std::map<char, int> binopPrecedence;
  if(!binopPrecedence.size())
  {
    // 1 is lowest precedence.
    binopPrecedence['<'] = 10;
    binopPrecedence['>'] = 10;
    binopPrecedence['&'] = 10;
    binopPrecedence['+'] = 20;
    binopPrecedence['-'] = 20;
    binopPrecedence['%'] = 10;
    binopPrecedence['/'] = 40;
    binopPrecedence['*'] = 40;  // highest.
  }
    
  int precedence = binopPrecedence[op];
  if (precedence <= 0) return -1;
  return precedence;
}

ast::Expr *parseLiteral(TokenIt &it, const TokenIt &end)
{
  ast::Expr *node = new ast::Literal((*it)->value);
  nextToken(it, end);
  return node;  
}

ast::Expr *parseExpr(TokenIt &it, const TokenIt &end);

ast::Expr *parseParenExpr(TokenIt &it, const TokenIt &end)
{
  nextToken(it, end);
  ast::Expr *node = parseExpr(it, end);
  if((*it)->type != Token::RPAREN)
    return errorE("Expected ')'");
        
  nextToken(it, end);
  return node;
}

ast::Expr *parseIdentifer(TokenIt &it, const TokenIt &end)
{
  std::string identifier = (*it)->value;
  Token *current = nextToken(it, end);
  if(!current || current->type != Token::LPAREN)
    return new ast::Identifier(identifier);

  current = nextToken(it, end);
  std::vector<ast::Expr *> args;
  if(current->type != Token::RPAREN)
    while(1)
    {
      ast::Expr *arg = parseExpr(it, end);
      if(!arg) return NULL;
      args.push_back(arg);

      current = *it;

      if(current->type == Token::RPAREN)
        break;
      else if(current->type != Token::COMMA)
        return errorE("Expected ')' or ','");

      current = nextToken(it, end);
    }

  nextToken(it, end);
  return new ast::CallExpr(identifier, args);
}

ast::Expr *parsePrimary(TokenIt &it, const TokenIt &end)
{
  switch((*it)->type)
  {
    case Token::IDENTIFIER: return parseIdentifer(it, end);
    case Token::NUMBER:     return parseLiteral(it, end);
    case Token::LPAREN:     return parseParenExpr(it, end);
    default:                return errorE("Unexpected token when expecting an expression");
  }
}

ast::Node *parseDim(TokenIt &it, const TokenIt &end)
{
  Token *current = nextToken(it, end);
  
  if(!current || current->type != Token::IDENTIFIER)
    return errorN("Expected identifier");

  ast::Node *dim = new ast::Dim(current->value);
  nextToken(it, end);
  return dim;
}

ast::Node *parseStatement(TokenIt &it, const TokenIt &end)
{
  switch((*it)->type)
  {
    case Token::DIM: return parseDim(it, end);
    default:         return parseExpr(it, end);
  }
}

ast::Expr *parseBinOpRHS(int exprPrecendence, ast::Expr *lhs, TokenIt &it, const TokenIt &end)
{
  while(1)
  {
    char binOp = it != end ? (*it)->value.c_str()[0] : 0;
    int precendence = opPrecedence(binOp);
    if(precendence < exprPrecendence)
      return lhs;
    nextToken(it, end);
    ast::Expr *rhs = parsePrimary(it, end);
    if(!rhs)
      return NULL;

    int nextPrecedence = opPrecedence(it != end ? (*it)->value.c_str()[0] : 0);
    if(precendence < nextPrecedence)
    {
      rhs = parseBinOpRHS(precendence + 1, rhs, it, end);
      if(!rhs)
        return NULL;
    }

    lhs = new ast::BinaryExpr(binOp, lhs, rhs);
  }
}

ast::Expr *parseExpr(TokenIt &it, const TokenIt &end)
{
  ast::Expr *lhs = parsePrimary(it, end);
  if(!lhs) return NULL;

  return parseBinOpRHS(NULL, lhs, it, end);
}

typedef std::list<ast::Node *> ASTNodeList;
typedef ASTNodeList::reverse_iterator ASTNodeListIt;

ASTNodeList *generateAST(const TokenList &tokens)
{
  ASTNodeList *ast = new ASTNodeList;
  ast::Node *node;
  TokenIt it = tokens.rbegin(), end = tokens.rend();
  while(it != end)
  {
    node = parseStatement(it, end);
    if(node)
      ast->push_back(node);
    else break;
  }
  return ast;
}

int main(int /*argc*/, char */*argv*/[])
{
  std::list<Token *> tokens;

  do
  {
    tokens.clear();
    std::cout << "> ";
    
    while(1)
    {
      Token *tok = getToken();
      if(!tok)
        break;
      tokens.push_front(tok);
    }
    
    if(tokens.size())
    {
      ASTNodeList *ast = generateAST(tokens);
      for(ASTNodeListIt it = ast->rbegin(); it != ast->rend(); it++)
        std::cout << (*it)->toString() << std::endl;
    }

    std::cin.clear();
  } while(tokens.size());
        
  return 0;
}
