#include <iostream>
#include <algorithm>
#include <vector>
#include <sstream>
#include <map>
using namespace std;
// Error catcher and error messages generator
class Failure {
    string message;
public:
    Failure(string message):message(move(message)){}
    const string &what() const {return message;}
};
// Types
enum TYPE {INT,INT_STAR,GOOD};
// Structure of the tree
struct Tree {
    string rule;
    vector<string> tokens;
    vector<Tree*> children;
    int num_children;
    TYPE type;
    ~Tree() {for(auto & x: children) delete x;}
};
// Build the tree
Tree *buildTree(string r, vector<string> & tokens){
    Tree * tree = new Tree();
    tree->rule = r;
    if(tokens.size() == 0) return tree;
    if(tokens.at(0) == "BOF" || tokens.at(0) == "BECOMES" || tokens.at(0) == "COMMA" ||
       tokens.at(0) == "ELSE" || tokens.at(0) == "EOF" || tokens.at(0) == "EQ" ||
       tokens.at(0) == "GE" || tokens.at(0) == "GT" || tokens.at(0) == "ID" ||
       tokens.at(0) == "IF" || tokens.at(0) == "INT" || tokens.at(0) == "LBRACE" ||
       tokens.at(0) == "LE" || tokens.at(0) == "LPAREN" || tokens.at(0) == "LT" ||
       tokens.at(0) == "MINUS" || tokens.at(0) == "NE" || tokens.at(0) == "NUM" ||
       tokens.at(0) == "PCT" || tokens.at(0) == "PLUS" || tokens.at(0) == "PRINTLN" ||
       tokens.at(0) == "RBRACE" || tokens.at(0) == "RETURN" || tokens.at(0) == "RPAREN" ||
       tokens.at(0) == "SEMI" || tokens.at(0) == "SLASH" || tokens.at(0) == "STAR" ||
       tokens.at(0) == "WAIN" || tokens.at(0) == "WHILE" || tokens.at(0) == "AMP" ||
       tokens.at(0) == "LBRACK" || tokens.at(0) == "RBRACK" || tokens.at(0) == "NEW" ||
       tokens.at(0) == "DELETE" || tokens.at(0) == "NULL"){
        tree->num_children = 0;
        tree->tokens = tokens;
        return tree;
    }else{
        tree->tokens = tokens;
        for(int i=1; i<tokens.size(); ++i){
            string next_rule;
            vector<string> next_tokens;
            string readline;
            getline(cin,readline);
            istringstream is(readline);
            string token;
            next_rule = readline;
            while(!is.eof()){
                is >> token;
                next_tokens.emplace_back(token);
            }
            tree->num_children = static_cast<int>(tree->tokens.size() - 1);
            tree->children.emplace_back(buildTree(next_rule,next_tokens));
        }
        return tree;
    }
}
// Output of the tree
string print(Tree * t){
    string result;
    if(t->num_children == 0){
        result = t->rule + '\n';
    }else{
        result = t->rule + '\n';
        for(auto & x: t->children){
            result.append(print(x));
        }
    }
    return result;
}
// Structure of Procedure
struct Procedure{
    string name;
    map<string,pair<int,TYPE>> Parameter;
    map<string,pair<int,TYPE>> Variable;
};
// Search whether there is duplicate inside the symbol table
bool searchDuplication(map<string,pair<int,TYPE>> &parameter,map<string,pair<int,TYPE>> &variable, string id){
    if(parameter.find(id) != parameter.end() || variable.find(id) != variable.end()){ return true;}
    else {return false;}
}
// Search inside procedure whether has duplicate function name
bool searchProcedure(vector<Procedure> & st, string s){
    if(st.size() == 0) return false;
    for(auto & x:st){
        if(x.name == s) return true;
    }
    return false;
}
// Go inside dcl and check the information
void checkDCL(map<string,pair<int,TYPE>> &parameter,map<string,pair<int,TYPE>> &variable,Tree * t,int &location){
    string id;
    TYPE type;
    if(t->children.at(0)->children.size() == 1){
        type = TYPE::INT;
    }else{
        type = TYPE::INT_STAR;
    }
    id = t->children.at(1)->tokens.at(1);
    //cout << id << endl;
    if(searchDuplication(parameter,variable,id)) throw Failure("ERROR: Cannot declare more than once");
    pair<int,TYPE> temp_pair;
    temp_pair = make_pair(location,type);
    parameter[id] = temp_pair;
    location -= 4;
}
// Go inside dcls and check the information
void checkDCLS(map<string,pair<int,TYPE>> &parameter,map<string,pair<int,TYPE>> &variable,Tree * t,int &location){
    if(t->rule == "dcls dcls dcl BECOMES NUM SEMI"|| t->rule == "dcls dcls dcl BECOMES NULL SEMI"){
        checkDCLS(parameter,variable,t->children.at(0),location);
        checkDCL(parameter,variable,t->children.at(1),location);
    }else{
        return;
    }
}
// Check params
void checkParams(map<string,pair<int,TYPE>> &parameter,map<string,pair<int,TYPE>> &variable,Tree * t,int &location){
    if(t->rule == "paramlist dcl") {
        checkDCL(parameter,variable,t->children.at(0),location);
    }else{
        checkDCL(parameter,variable,t->children.at(0),location);
        checkParams(parameter,variable,t->children.at(2),location);
    }
}
//Build Symbol Table
void buildST(vector<Procedure> & st, Tree * t, string cur){
    if(t->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        int location = 0;
        string fname = t->children.at(1)->tokens.at(1);
        if(searchProcedure(st,fname)) throw Failure("ERROR: Function name has been declared");
        Procedure temp;
        temp.name = fname;
        Tree * subtree;
        subtree = t->children.at(3);
        if(subtree->num_children != 0){
            checkParams(temp.Parameter,temp.Variable,subtree->children.at(0),location);
        }
        subtree = t->children.at(6);
        checkDCLS(temp.Variable,temp.Parameter,subtree,location);
        st.emplace_back(temp);
        cur = fname;
        buildST(st,t->children.at(7),cur);
        buildST(st,t->children.at(9),cur);
    }else if(t->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        int location = 0;
        Procedure temp;
        temp.name = "wain";
        Tree * subtree;
        subtree = t->children.at(3);
        checkDCL(temp.Parameter,temp.Variable,subtree,location);
        subtree = t->children.at(5);
        checkDCL(temp.Parameter,temp.Variable,subtree,location);
        subtree = t->children.at(8);
        checkDCLS(temp.Variable,temp.Parameter,subtree,location);
        st.emplace_back(temp);
        cur = "wain";
        buildST(st,t->children.at(9),cur);
        buildST(st,t->children.at(11),cur);
    }else if(t->rule == "factor ID LPAREN RPAREN" || t->rule == "factor ID LPAREN arglist RPAREN"){
        Tree * subtree = t->children.at(0);
        string name = subtree->tokens.at(1);
        if(name == "wain") throw Failure("ERROR: Cannot call wain function");
        bool found = false;
        for(auto & x: st){
            if(x.name == name) found = true;
            if(x.name == cur){
                if(x.Parameter.find(name) != x.Parameter.end() || x.Variable.find(name) != x.Variable.end()){
                    throw Failure("ERROR: Function name was declared before");
                }
            }
        }if(!found) throw Failure("ERROR: Used an undefined variable");
    }else{
        for (vector<Tree *>::iterator it = t->children.begin(); it != t->children.end(); ++it) {
            buildST(st, *it, cur);
        }
    }
}
void checkVar(Procedure & p, Tree * t, vector<Procedure> & st){
    if(t->tokens.at(0) == "lvalue"){
        if(t->num_children == 1){
            string name = t->children.at(0)->tokens.at(1);
            Tree * subtree;
            subtree = t->children.at(0);
            if(searchDuplication(p.Parameter,p.Variable,name)){
                return;
            }else{
                throw Failure("ERROR: Used an undeclared variable");
            }
        }else{
            for(vector<Tree *>::iterator it = t->children.begin(); it != t->children.end(); ++it){
                checkVar(p,*it,st);
            }
        }
    }else if(t->tokens.at(0) == "factor"){
        for(vector<Tree *>::iterator it = t->children.begin(); it != t->children.end(); ++it){
            if((*it)->tokens.at(0) == "ID"){
                string name = (*it)->tokens.at(1);
                if(searchDuplication(p.Parameter,p.Variable,name) || searchProcedure(st,name)){
                    return;
                }else{
                    throw Failure("ERROR: Used an undeclared variable");
                }
            }else{
                checkVar(p,*it,st);
            }
        }
    }else{
        for(vector<Tree *>::iterator it = t->children.begin(); it != t->children.end(); ++it){
            checkVar(p,*it,st);
        }
    }
}
// Check whether variables are valid when using
void checkError(vector<Procedure> & st,Tree * t){
    if(t->num_children == 0) return;
    if(t->num_children != 1){
        Tree * temp;
        string fname;
        temp = t->children.at(0);
        fname = t->children.at(0)->children.at(1)->tokens.at(1);
        for(auto & x:st){
            string name = x.name;
            if(fname == name){
                checkVar(x,temp,st);
            }
        }
        temp = t->children.at(1);
        checkError(st,temp);
    }else{
        Tree * temp;
        string fname = "wain";
        temp = t->children.at(0);
        for(auto & x:st){
            string name = x.name;
            if(fname == name){
                checkVar(x,temp,st);
            }
        }
    }
}
// Print Symbol Table
void printST(vector<Procedure> &st) {
    for(auto & x:st){
        if(x.name == "wain"){
            cerr << "wain";
            for(auto & y:x.Parameter){
                cerr << " ";
                if(y.second.second == TYPE::INT){
                    cerr << "int";
                }else{
                    cerr << "int*";
                }
            }
            cerr << endl;
            for(auto & y:x.Parameter){
                cerr << y.first << " ";
                if(y.second.second == TYPE::INT){
                    cerr << "int" << endl;
                }else{
                    cerr << "int*" << endl;
                }
            }
            for(auto & y:x.Variable){
                cerr << y.first << " ";
                if(y.second.second == TYPE::INT){
                    cerr << "int" << endl;
                }else{
                    cerr << "int*" << endl;
                }
            }
        }else{
            cerr << x.name;
            for(auto & y:x.Parameter){
                cerr << " ";
                if(y.second.second == TYPE::INT){
                    cerr << "int";
                }else{
                    cerr << "int*";
                }
            }
            cerr << endl;
            for(auto & y:x.Parameter){
                cerr << y.first << " ";
                if(y.second.second == TYPE::INT){
                    cerr << " int" << endl;
                }else{
                    cerr << " int*" << endl;
                }
            }
            for(auto & y:x.Variable){
                cerr << y.first << " ";
                if(y.second.second == TYPE::INT){
                    cerr << "int" << endl;
                }else{
                    cerr << "int*" << endl;
                }
            }
            cerr << endl;
        }
    }
}
//Store the type of arguments
vector<TYPE> arguments;
// Check whether parameters are same
bool same(vector<TYPE> & p1, map<string,pair<int,TYPE>> & p2){
    if(p1.size() != p2.size()) return false;
    map<string,pair<int,TYPE>>::iterator y = p2.begin();
    for(auto & x:p1){
        TYPE type;
        if(x == TYPE::INT){ type = TYPE::INT;}
        else{ type = TYPE::INT_STAR;}
        if(type != (*y).second.second) return false;
        ++ y;
    }
    return true;
}
// Check types and errors
void checkType(vector<Procedure> & st, Tree * t, string current){
    string cur_rule;
    cur_rule = t->rule;
    //Literals and Identifiers
    if(cur_rule == "factor NUM"){
        t->type = TYPE::INT;
    }else if(cur_rule == "factor NULL"){
        t->type = TYPE::INT_STAR;
    }else if(cur_rule == "factor ID" || cur_rule == "lvalue ID"){
        string name = t->children.at(0)->tokens.at(1);
        TYPE type;
        for(auto & x: st){
            if(x.name == current){
                if(x.Parameter.find(name) != x.Parameter.end()){
                    type = x.Parameter.at(name).second;
                    if(type == TYPE::INT){
                        t->type = TYPE::INT;
                    }else{
                        t->type = TYPE::INT_STAR;
                    }
                }
                if(x.Variable.find(name) != x.Variable.end()){
                    type = x.Variable.at(name).second;
                    if(type == TYPE::INT){
                        t->type = TYPE::INT;
                    }else{
                        t->type = TYPE::INT_STAR;
                    }
                }
            }
        }
    }// Parenthesized expressions
    else if(cur_rule == "lvalue LPAREN lvalue RPAREN" || cur_rule == "factor LPAREN expr RPAREN"){
        checkType(st,t->children.at(1),current);
        t->type = t->children.at(1)->type;
    }// Pointers
    else if(cur_rule == "factor AMP lvalue"){
        checkType(st,t->children.at(1),current);
        if(t->children.at(1)->type == TYPE::INT){
            t->type = TYPE::INT_STAR;
        }else{
            throw Failure("ERROR: Cannot get the address of a pointer");
        }
    }else if(cur_rule == "factor STAR factor" || cur_rule == "lvalue STAR factor"){
        checkType(st,t->children.at(1),current);
        if(t->children.at(1)->type == TYPE::INT_STAR){
            t->type = TYPE::INT;
        }else{
            throw Failure("ERROR: Cannot dereference an non pointer");
        }
    }else if(cur_rule == "factor NEW INT LBRACK expr RBRACK"){
        checkType(st,t->children.at(3),current);
        if(t->children.at(3)->type == TYPE::INT){
            t->type = TYPE::INT_STAR;
        }else{
            throw Failure("ERROR: Fail to allocate a new pointer");
        }
    }// Addition
    else if(cur_rule == "expr expr PLUS term"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        if(t->children.at(0)->type == TYPE::INT && t->children.at(2)->type == TYPE::INT){
            t->type = TYPE::INT;
        }else if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT_STAR){
            throw Failure("ERROR: Cannot add two pointers");
        }else{
            t->type = TYPE::INT_STAR;
        }
    }//Substraction
    else if(cur_rule == "expr expr MINUS term"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        if(t->children.at(0)->type == TYPE::INT && t->children.at(2)->type == TYPE::INT){
            t->type = TYPE::INT;
        }else if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT){
            t->type = TYPE::INT_STAR;
        }else if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT_STAR){
            t->type = TYPE::INT;
        }else{
            throw Failure("ERROR: Pointer cannot be subtrahend by integer");
        }
    }//Multiplication and division
    else if(cur_rule == "term term STAR factor" || cur_rule == "term term SLASH factor" || cur_rule == "term term PCT factor"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        if(t->children.at(0)->type == TYPE::INT && t->children.at(2)->type == TYPE::INT){
            t->type = TYPE::INT;
        }else{
            throw Failure("ERROR: Cannot do multiplication and division with pointer");
        }
    }// Procedure calls
    else if(cur_rule == "factor ID LPAREN arglist RPAREN"){
        checkType(st,t->children.at(2),current);
        string fname = t->children.at(0)->tokens.at(1);
        if(searchProcedure(st,fname)){
            reverse(arguments.begin(),arguments.end());
            for(auto & x:st){
                if(x.name == fname){
                    if(same(arguments,x.Parameter)){
                        t->type = TYPE::INT;
                        arguments.clear();
                    }else{
                        throw Failure("ERROR: Called function do not match parameters");
                    }
                }
            }
        }else{
            throw Failure("ERROR: Used undeclared function");
        }
        
    }else if(cur_rule == "arglist expr"){
        checkType(st,t->children.at(0),current);
        if(t->children.at(0)->type == TYPE::INT && t->children.at(2)->type == TYPE::INT_STAR){
            arguments.emplace_back(t->children.at(0)->type);
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Wrong type");
        }
    }else if(cur_rule == "arglist expr COMMA arglist"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        TYPE temp = t->children.at(0)->type;
        if((temp == TYPE::INT || temp == TYPE::INT_STAR) && t->children.at(2)->type == TYPE::GOOD){
            t->type = TYPE::GOOD;
            arguments.emplace_back(temp);
        }else{
            throw Failure("ERROR: Something is wrong");
        }
    }else if(cur_rule == "factor ID LPAREN RPAREN") {
        string fname = t->children.at(0)->tokens.at(1);
        if(searchProcedure(st,fname)){
            for(auto & x:st){
                if(x.name == fname){
                    if(x.Parameter.size() != 0){
                        throw Failure("ERROR: Reuqire parameters when calling function");
                    }else{
                        t->type = TYPE::INT;
                    }
                }
            }
        }else{
            throw Failure("ERROR: Used an undeclared function");
        }
    }// Comparisons
    else if(cur_rule == "test expr EQ expr" || cur_rule == "test expr NE expr" || cur_rule == "test expr LT expr" ||
            cur_rule == "test expr LE expr" || cur_rule == "test expr GE expr" || cur_rule == "test expr GT expr"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        if(t->children.at(0)->type == t->children.at(2)->type){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Cannot compare with different types");
        }
    }//Control flow
    else if(cur_rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
        checkType(st,t->children.at(2),current);
        checkType(st,t->children.at(5),current);
        if(t->children.at(2)->type == TYPE::GOOD && t->children.at(5)->type == TYPE::GOOD){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: While loop invalid");
        }
    }else if(cur_rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){
        checkType(st,t->children.at(2),current);
        checkType(st,t->children.at(5),current);
        checkType(st,t->children.at(9),current);
        if(t->children.at(2)->type == TYPE::GOOD && t->children.at(5)->type == TYPE::GOOD
           && t->children.at(9)->type == TYPE::GOOD){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: If else statement invalid");
        }
    }// Deallocation
    else if(cur_rule == "statement DELETE LBRACK RBRACK expr SEMI"){
        checkType(st,t->children.at(3),current);
        if(t->children.at(3)->type == TYPE::INT_STAR){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Deallocation failed");
        }
    }//Printing
    else if(cur_rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
        checkType(st,t->children.at(2),current);
        if(t->children.at(2)->type == TYPE::INT){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Printing failed");
        }
    }// Assignment
    else if(cur_rule == "statement lvalue BECOMES expr SEMI"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(2),current);
        if(t->children.at(0)->type == t->children.at(2)->type){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Assignment failed");
        }
    }//Sequencing
    else if(cur_rule == "statements"){
        t->type = TYPE::GOOD;
    }else if(cur_rule == "statements statement statements"){
        checkType(st,t->children.at(0),current);
        checkType(st,t->children.at(1),current);
        if(t->children.at(0)->type == TYPE::GOOD && t->children.at(1)->type == TYPE::GOOD){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Sequencing failed");
        }
    }// Decl'ns
    else if(cur_rule == "dcls"){
        t->type = TYPE::GOOD;
    }else if(cur_rule == "dcls dcls dcl BECOMES NUM SEMI"){
        checkType(st,t->children.at(0),current);
        if(t->children.at(0)->type == TYPE::GOOD &&
           t->children.at(1)->children.at(0)->rule == "type INT"){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Decl'ns NUM failed");
        }
    }else if(cur_rule == "dcls dcls dcl BECOMES NULL SEMI"){
        checkType(st,t->children.at(0),current);
        if(t->children.at(0)->type == TYPE::GOOD &&
           t->children.at(1)->children.at(0)->rule == "type INT STAR"){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Decl'ns NULL failed");
        }
    }// Procedure
    else if(cur_rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        current = t->children.at(1)->tokens.at(1);
        checkType(st,t->children.at(6),current);
        checkType(st,t->children.at(7),current);
        checkType(st,t->children.at(9),current);
        if(t->children.at(6)->type == TYPE::GOOD && t->children.at(7)->type == TYPE::GOOD
           && t->children.at(9)->type == TYPE::INT){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Failed when calling functions");
        }
    }else if(cur_rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        current = "wain";
        checkType(st,t->children.at(8),current);
        checkType(st,t->children.at(9),current);
        checkType(st,t->children.at(11),current);
        if(t->children.at(8)->type == TYPE::GOOD && t->children.at(9)->type == TYPE::GOOD &&
           t->children.at(11)->type == TYPE::INT && t->children.at(5)->children.at(0)->rule == "type INT"){
            t->type = TYPE::GOOD;
        }else{
            throw Failure("ERROR: Failed when calling wain");
        }
    }else{
        for(vector<Tree*>::iterator it = t->children.begin(); it != t->children.end(); it++){
            checkType(st,(*it),current);
            t->type = (*it)->type;
        }
    }
}

int countwhile = 0;
int countif = 0;
int countdelete = 0;
// Generate ASM
void ASMgen(vector<Procedure> &st, Tree *t, string cur){
    if(t->rule == "start BOF procedures EOF"){
        ASMgen(st,t->children.at(1),cur);
    }else if(t->rule == "procedures main"){
        ASMgen(st,t->children.at(0),cur);
    }else if(t->rule== "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        cur = "wain";
        // Puch to stack
        cout << ".import init" << endl;
        cout << ".import new" << endl;
        cout << ".import delete" << endl;
        cout << ".import print" << endl;
        cout << ";;Prolog" << endl;
        cout << "lis $4" << endl << ".word 4" << endl;
        cout << "lis $11" << endl << ".word 1" << endl;
        cout << "sub $29, $30, $4" << endl;
        int size = 0;
        for(auto & x:st){
            if(x.name == "wain"){
                size = static_cast<int>(x.Parameter.size() + x.Variable.size());
                size = size * 4;
                break;
            }
        }
        cout << "lis $8" << endl;
        cout << ".word " << size << endl;
        cout << "sub $30, $30, $8" << endl;
        cout << "sw $1, 0($29)" << endl;
        cout << "sw $2, -4($29)" << endl;
        Tree * subtree = t->children.at(3);
        if(subtree->children.at(0)->tokens.size() == 2){
            cout << "add $2, $0, $0" << endl;
        }
        // Push Register
        cout << "sw $31, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        cout << "lis $5" << endl;
        cout << ".word init" << endl;
        cout << "jalr $5" << endl;
        // Pop Register
        cout << "lw $31, 0($30)" << endl;
        cout << "add $30, $30, $4" << endl;
        ASMgen(st,t->children.at(8),cur);
        ASMgen(st,t->children.at(9),cur);
        ASMgen(st,t->children.at(11),cur);
        cout << "add $30, $29, $4" << endl;
        cout << "jr $31" << endl;
    }else if(t->rule == "dcls" || t->rule == "statements"){
        return;
    }else if(t->rule == "expr term" || t->rule == "term factor"){
        ASMgen(st,t->children.at(0),cur);
    }else if(t->rule == "factor ID"){
        string id = t->children.at(0)->tokens.at(1);
        int offset;
        for(auto & x:st){
            if(x.name == cur){
                if(x.Parameter.find(id) != x.Parameter.end()){
                    offset = x.Parameter[id].first;
                }else{
                    offset = x.Variable[id].first;
                }
                cout << "lw $3, " << offset << "($29)" << endl;
                break;
            }
        }
    }else if(t->rule == "factor LPAREN expr RPAREN"){
        ASMgen(st,t->children.at(1),cur);
    }else if(t->rule == "expr expr PLUS term"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT){
            cout << "mult $3, $4" << endl;
            cout << "mflo $3" << endl;
        }else if(t->children.at(0)->type == TYPE::INT && t->children.at(2)->type == TYPE::INT_STAR){
            cout << "mult $5, $4" << endl;
            cout << "mflo $5" << endl;
        }
        cout << "add $3, $5, $3" << endl;
    }else if(t->rule == "expr expr MINUS term"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT){
            cout << "mult $3, $4" << endl;
            cout << "mflo $3" << endl;
        }
        cout << "sub $3, $5, $3" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR && t->children.at(2)->type == TYPE::INT_STAR){
            cout << "div $3, $4" << endl;
            cout << "mflo $3" << endl;
        }
    }else if(t->rule == "term term STAR factor"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        cout << "mult $5, $3" << endl;
        cout << "mflo $3" << endl;
    }else if(t->rule == "term term SLASH factor"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        cout << "div $5, $3" << endl;
        cout << "mflo $3" << endl;
    }else if(t->rule == "term term PCT factor"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        cout << "div $5, $3" << endl;
        cout << "mfhi $3" << endl;
    }else if(t->rule == "factor NUM"){
        cout << "lis $3" << endl;
        cout << ".word " << t->children.at(0)->tokens.at(1) << endl;
    }else if(t->rule == "statements statements statement"){
        ASMgen(st,t->children.at(0),cur);
        ASMgen(st,t->children.at(1),cur);
    }else if(t->rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
        ASMgen(st,t->children.at(2),cur);
        cout << "add $1, $3, $0" << endl;
        // Push Register 31
        cout << "sw $31, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        cout << "lis $10" << endl;
        cout << ".word print" << endl;
        cout << "jalr $10" << endl;
        // Pop Register 31
        cout << "lw $31, 0($30)" << endl;
        cout << "add $30, $30, $4" << endl;
        cout << "lw $1, 0($29)" << endl;
    }else if(t->rule == "dcls dcls dcl BECOMES NUM SEMI"){
        ASMgen(st,t->children.at(0),cur);
        string id = t->children.at(1)->children.at(1)->tokens.at(1);
        int offset = 0;
        for(auto & x: st){
            if(x.name == cur){
                offset = x.Variable[id].first;
                break;
            }
        }
        cout << "lis $3" << endl;
        cout << ".word " << t->children.at(3)->tokens.at(1) << endl;
        cout << "sw $3, " << offset << "($29)" << endl;
    }else if(t->rule == "statement lvalue BECOMES expr SEMI"){
        ASMgen(st,t->children.at(2),cur);
        string rule = t->children.at(0)->rule;
        Tree * subtree = t->children.at(0);
        while(subtree->rule == "lvalue LPAREN lvalue RPAREN"){
            subtree = subtree->children.at(1);
        }
        if(subtree->rule == "lvalue ID"){
            string id = subtree->children.at(0)->tokens.at(1);
            int offset = 0;
            for(auto & x: st){
                if(x.name == cur){
                    if(x.Parameter.find(id) != x.Parameter.end()){
                        offset = x.Parameter[id].first;
                    }else{
                        offset = x.Variable[id].first;
                        break;
                    }
                }
            }
            cout << "sw $3, " << offset << "($29)" << endl;
        }else if(subtree->rule == "lvalue STAR factor"){
            // Push Register 3
            cout << "sw $3, -4($30)" << endl;
            cout << "sub $30, $30, $4" << endl;
            ASMgen(st,subtree->children.at(1),cur);
            // Pop to Register 5
            cout << "add $30, $30, $4" << endl;
            cout << "lw $5, -4($30)" << endl;
            cout << "sw $5, 0($3)" << endl;
        }
    }else if(t->rule == "test expr LT expr" || t->rule == "test expr GE expr"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR){
            cout << "sltu $3, $5, $3" << endl;
        }else{
            cout << "slt $3, $5, $3" << endl;
        }
        if(t->rule == "test expr GE expr"){
            cout << "sub $3, $11, $3" << endl;
        }
    }else if(t->rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
        string num = to_string(countwhile);
        ++countwhile;
        cout << "whileloop" << num << ":" << endl;
        ASMgen(st,t->children.at(2),cur);
        cout << "beq $3, $0, endw" << num <<  endl;
        ASMgen(st,t->children.at(5),cur);
        cout << "beq $0, $0, whileloop" << num << endl;
        cout << "endw" << num << ":" << endl;
    }else if(t->rule == "test expr GT expr" || t->rule == "test expr LE expr"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop to Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR){
            cout << "sltu $3, $3, $5" << endl;
        }else{
            cout << "slt $3, $3, $5" << endl;
        }
        if(t->rule == "test expr LE expr"){
            cout << "sub $3, $11, $3" << endl;
        }
    }else if(t->rule == "test expr EQ expr" || t->rule == "test expr NE expr"){
        ASMgen(st,t->children.at(0),cur);
        // Push Register 3
        cout << "sw $3, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        ASMgen(st,t->children.at(2),cur);
        // Pop Register 5
        cout << "add $30, $30, $4" << endl;
        cout << "lw $5, -4($30)" << endl;
        if(t->children.at(0)->type == TYPE::INT_STAR){
            cout << "sltu $6, $5, $3" << endl;
            cout << "sltu $7, $3, $5" << endl;
        }else{
            cout << "slt $6, $5, $3" << endl;
            cout << "slt $7, $3, $5" << endl;
        }
        cout << "add $3, $6, $7" << endl;
        if(t->rule == "test expr EQ expr"){
            cout << "sub $3, $11, $3" << endl;
        }
    }else if(t->rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){
        ASMgen(st,t->children.at(2),cur);
        string num = to_string(countif);
        countif ++;
        cout << "beq $0, $3, ifstatement" << num << endl;
        ASMgen(st,t->children.at(5),cur);
        cout << "beq $0, $0, endi" << num << endl;
        cout << "ifstatement" << num << ":" << endl;
        ASMgen(st,t->children.at(9),cur);
        cout << "endi" << num << ":" << endl;
    }else if(t->rule == "dcls dcls dcl BECOMES NULL SEMI"){
        ASMgen(st,t->children.at(0),cur);
        string id = t->children.at(1)->children.at(1)->tokens.at(1);
        int offset = 0;
        for(auto & x: st){
            if(x.name == cur){
                offset = x.Variable[id].first;
                break;
            }
        }
        cout << "add $3, $0, $11" << endl;
        cout << "sw $3, " << offset << "($29)" << endl;
    }else if(t->rule == "factor NULL"){
        cout << "add $3, $0, $11" << endl;
    }else if(t->rule == "factor AMP lvalue"){
        Tree * subtree = t->children.at(1);
        while (t->rule == "lvalue LPAREN lvalue RPAREN"){
            subtree = subtree->children.at(1);
        }
        if(t->rule == "lvalue ID"){
            string id = subtree->children.at(0)->tokens.at(1);
            int offset = 0;
            for(auto & x: st){
                if(x.name == cur){
                    if(x.Parameter.find(id) == x.Parameter.end()){
                        offset = x.Variable[id].first;
                    }else{
                        offset = x.Parameter[id].first;
                    }
                    break;
                }
            }
            cout << "lis $3" << endl;
            cout << ".word " << offset << endl;
            cout << "add $3, $3, $29" << endl;
        }else if(subtree->rule == "lvalue STAR factor"){
            ASMgen(st,subtree->children.at(1),cur);
        }
    }else if(t->rule == "factor STAR factor"){
        ASMgen(st,t->children.at(1),cur);
        cout << "lw $3, 0($3)" << endl;
    }else if(t->rule == "factor NEW INT LBRACK expr RBRACK"){
        ASMgen(st,t->children.at(3),cur);
        cout << "add $1, $3, $0" << endl;
        // Push Register 31
        cout << "sw $31, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        // Allocate
        cout << "lis $5" << endl;
        cout << ".word new" << endl;
        cout << "jalr $5" << endl;
        // Pop Register 31
        cout << "lw $31, 0($30)" << endl;
        cout << "add $30, $30, $4" << endl;
        cout << "bne $3, $0, 1" << endl;
        cout << "add $3, $0, $11" << endl;
    }else if(t->rule == "statement DELETE LBRACK RBRACK expr SEMI"){
        ASMgen(st,t->children.at(3),cur);
        ++ countdelete;
        string num = to_string(countdelete);
        cout << "beq $3, $11, skip" << num << endl;
        cout << "add $1, $3, $0" << endl;
        // Push Register 31
        cout << "sw $31, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        // Deallocate
        cout << "lis $5" << endl;
        cout << ".word delete" << endl;
        cout << "jalr $5" << endl;
        // Pop Register 31
        cout << "lw $31, 0($30)" << endl;
        cout << "add $30, $30, $4" << endl;
        cout << "skip" << num  << ":" << endl;
    }else if(t->rule == "procedures procedure procedures"){
        ASMgen(st,t->children.at(1),cur);
        ASMgen(st,t->children.at(0),cur);
    }else if(t->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        cur = t->children.at(1)->tokens.at(1);
        if(t->children.at(3)->rule == "params paramlist"){
            for(auto & x:st){
                if(x.name == cur){
                    int location;
                    int size = static_cast<int>(4*(x.Parameter.size() + x.Variable.size()));
                    for(auto & y: x.Parameter){
                        location = y.second.first;
                        location += size;
                        y.second.first = location;
                    }
                    for(auto & z: x.Variable){
                        location = z.second.first;
                        location += size;
                        z.second.first = location;
                    }
                }
            }
        }
        cout << "Factor" << cur << ":" << endl;
        cout << "sub $29, $30, $4" << endl;
        ASMgen(st,t->children.at(6),cur);
        ASMgen(st,t->children.at(7),cur);
        ASMgen(st,t->children.at(9),cur);
        cout << "add $30, $29, $4" << endl;
        cout << "jr $31" << endl;
    }else if(t->rule == "factor ID LPAREN RPAREN"){
        string fname = "Factor" + t->children.at(0)->tokens.at(1);
        // Push Register 29
        cout << "sw $29, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        // Push Register 31
        cout << "sw $31, -4($30)" << endl;
        cout << "sub $30, $30, $4" << endl;
        cout << "lis $5" << endl;
        cout << ".word " << fname << endl;
        cout << "jalr $5" << endl;
        // Pop Register 31
        cout << "lw $31, 0($30)" << endl;
        cout << "add $30, $30, $4" << endl;
        // Pop Register 29
        cout << "lw $29, -4($30)" << endl;
        cout << "add $30, $30, $4" << endl;
    }
}
// Main
int main(){
    try{
        // Scan
        string rule;
        vector<string> tokens;
        Tree * root;
        string readin;
        getline(cin,readin);
        istringstream is(readin);
        string token;
        rule = readin;
        while(!is.eof()){
            is >> token;
            tokens.emplace_back(token);
        }
        // Build tree
        root = buildTree(rule,tokens);
        string result;
        result = print(root);
        // Symbol Table
        vector<Procedure> symbol_table;
        buildST(symbol_table,root,"");
        checkError(symbol_table,root->children.at(1));
        checkType(symbol_table,root,"");
        ASMgen(symbol_table,root,"");
    }catch (Failure &f) {
        cerr << f.what() << endl;
        return 1;
    }
    return 0;
}
