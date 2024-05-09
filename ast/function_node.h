#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function nodes.
  */
  class function_node : public cdk::expression_node {
    cdk::sequence_node *_arguments, *_declarations, *_instructions;
    bool _is_main;

  public:
    function_node(int line_no, std::shared_ptr<cdk::basic_type> return_type, cdk::sequence_node *arguments, cdk::sequence_node *declarations, cdk::sequence_node *instructions, bool is_main) :
      cdk::expression_node(line_no), _arguments(arguments), _declarations(declarations), _instructions(instructions), _is_main(false) {
        std::vector<std::shared_ptr<cdk::basic_type>> arg_types;

        for (auto node : _arguments->nodes()) {
          arg_types.push_back(dynamic_cast<cdk::typed_node*>(node)->type());
        }

        type(cdk::functional_type::create(arg_types, return_type));
      }


    function_node(int line_no, cdk::sequence_node *declarations, cdk::sequence_node *instructions) : cdk::expression_node(line_no), _declarations(declarations), _instructions(instructions), _is_main(true) {
      type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }

    cdk::sequence_node *arguments() { return _arguments; }
    cdk::sequence_node *declarations() { return _declarations; }
    cdk::sequence_node *instructions() { return _instructions; }
    bool is_main() { return _is_main; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_node(this, level);
    }
  };
} // til

#endif