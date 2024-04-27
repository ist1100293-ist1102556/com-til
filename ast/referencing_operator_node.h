#ifndef __TIL_AST_REFERENCING_OPERATOR_NODE_H__
#define __TIL_AST_REFERENCING_OPERATOR_NODE_H__

namespace til {
    /**
   * Class for describing the node that represents the '?' operator nodes.
   */
  class referencing_operator_node: public cdk::expression_node {
    cdk::lvalue_node *_lval;

    public: 
        referencing_operator_node(int lineno, cdk::lvalue_node *lval) : cdk::expression_node(lineno), _lval(lval) {
        }

        void accept(basic_ast_visitor *sp, int level) {
            sp->do_referencing_operator_node(this, level);
        }

        cdk::lvalue_node *lval() {
            return _lval;
        }
  };
} //til

#endif