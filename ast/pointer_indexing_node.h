#ifndef __TIL_AST_POINTER_INDEXING_NODE_H__
#define __TIL_AST_POINTER_INDEXING_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing pointer indexing nodes.
   */
  class pointer_indexing_node : public cdk::lvalue_node {
    cdk::expression_node *_pointer, *_index;

  public:
    pointer_indexing_node(int lineno, cdk::expression_node *pointer, cdk::expression_node *index) :
        cdk::lvalue_node(lineno), _pointer(pointer), _index(index) {
    }

    cdk::expression_node *pointer() {
      return _pointer;
    }

    cdk::expression_node *index() {
      return _index;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_pointer_indexing_node(this, level);
    }

  };

} // til

#endif
