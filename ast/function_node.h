#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include <cdk/ast/typed_node.h>

namespace til {

  /**
   * Class for describing function nodes.
  */
  class function_node : public cdk::typed_node {
    cdk::sequence_node *_arguments;
    til::block_node *_block;
    bool _is_main;

    
  public:
    function_node(int line_no, std::shared_ptr<cdk::basic_type> return_type, cdk::sequence_node *arguments, til::block_node *block, bool is_main) :
      cdk::typed_node(line_no), _arguments(arguments), _block(block), _is_main(false) {
        std::vector<std::shared_ptr<cdk::basic_type>> arg_types;

        for (auto node : _arguments->nodes()) {
          arg_types.push_back(dynamic_cast<cdk::typed_node*>(node)->type());
        }

        type(cdk::functional_type::create(arg_types, return_type));
      }


    function_node(int line_no, til::block_node *_block) : cdk::typed_node(line_no), _block(_block), _is_main(true) {
      type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }

    cdk::sequence_node *arguments() { return _arguments; }
    til::block_node *block() { return _block; }
    bool is_main() { return _is_main; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_node(this, level);
    }
  };
} // til

#endif