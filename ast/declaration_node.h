#ifndef __TIL_AST_DECLARATION_NODE_H__
#define __TIL_AST_DECLARATION_NODE_H__

#include <cdk/ast/typed_node.h>

namespace til {

  /**
   * Class for describing declaration nodes.
  */
  class declaration_node : public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::expression_node *_initial;
    
  public:
    declaration_node(int line_no, int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &identifier, cdk::expression_node *initial) :
      cdk::typed_node(line_no), _qualifier(qualifier), _identifier(identifier), _initial(initial) {
        this->type(type);
    }

    int qualifier() { return _qualifier; }
    const std::string &identifier() { return _identifier; }
    cdk::expression_node *initial() { return _initial; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_declaration_node(this, level);
    }
  };
} // til

#endif