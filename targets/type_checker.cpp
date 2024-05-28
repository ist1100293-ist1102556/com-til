#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  //Check the level
  for(size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}
void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in argument of unary expression");
  }
}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in arguments of binary expression");
  }
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in arguments of binary expression");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");

  // in Simple, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);

  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }

}

void til::type_checker::ID_operation(cdk::binary_operation_node *node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);

  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else {
    throw node->lineno() + "wrong types in arguments of binary expression";
  }
}

void til::type_checker::PID_operation(cdk::binary_operation_node *node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl);
  if (!node->left()->is_typed(cdk::TYPE_INT) && !node->left()->is_typed(cdk::TYPE_POINTER)
      && !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    throw node->lineno() + "wrong type in left argument of binary expression";
  }

  node->right()->accept(this, lvl);
  if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)
      && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    throw node->lineno() + "wrong type in left argument of binary expression";
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw node->left()->lineno() + "wrong type in left argument of mod expression";
  }

  node->right()->accept(this, lvl);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw node->right()->lineno() + "wrong type in right argument of mod expression";
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  ID_operation(node, lvl);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);
  if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
    node->rvalue()->type(node->lvalue()->type());
  }

  if (node->lvalue()->is_typed(cdk::TYPE_POINTER) && node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto lvalue_pointer = cdk::reference_type::cast(node->lvalue()->type());
    auto rvalue_pointer = cdk::reference_type::cast(node->rvalue()->type());

    if (rvalue_pointer->referenced()->name() == cdk::TYPE_UNSPEC ||
        rvalue_pointer->referenced()->name() == cdk::TYPE_VOID ||
        lvalue_pointer->referenced()->name() == cdk::TYPE_VOID) {
          node->rvalue()->type(node->lvalue()->type());
        }
  }
  
  if (compare_types(node->lvalue()->type(), node->rvalue()->type(), true)) {
    node->type(node->lvalue()->type());
  } else {
    throw std::string("wrong types in assignment");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_node(til::function_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  //Check the level
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of loop expression");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of if expression");
  }
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of if else expression");
  }
}

//---------------------------------------------------------------------------
void til::type_checker::do_nullptr_node(til::nullptr_node * const node, int lvl) {
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------
void til::type_checker::do_stop_node(til::stop_node * const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------
void til::type_checker::do_next_node(til::next_node * const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------
void til::type_checker::do_return_node(til::return_node * const node, int lvl) {
  auto sym = _symtab.find("@");
  if (sym == nullptr) {
    throw std::string("return statement outside function");
  }
  auto return_type = cdk::functional_type::cast(sym->type())->output(0);

  if (node->value() == nullptr) {
    if (return_type->name() != cdk::TYPE_VOID) {
      throw std::string("return type mismatch");
    }
    return;
  }

  node->value()->accept(this, lvl);
  if (!compare_types(return_type, node->value()->type(), true)) {
    throw std::string("return type mismatch");
  }
}

// Receives 2 types that are not unspec, and checks if they are compatible
// if cov is true, then it checks for covariant types.
bool til::type_checker::compare_types(std::shared_ptr<cdk::basic_type> left, std::shared_ptr<cdk::basic_type> right, bool cov) {
  if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
    return false;
  } else if (left->name() == cdk::TYPE_FUNCTIONAL && right->name() == cdk::TYPE_FUNCTIONAL) {
    auto left_func = cdk::functional_type::cast(left);
    auto right_func = cdk::functional_type::cast(right);

    if (left_func->input_length() != right_func->input_length() ||
        left_func->output_length() != right_func->output_length() ) {
          return false;
    }

    for (size_t i = 0; i < left_func->output_length(); i++) {
      if (!compare_types(left_func->output(i), right_func->output(i), cov)) {
        return false;
      }
    }

    for (size_t i = 0; i < left_func->input_length(); i++) {
      // We invert the order of the arguments, because in the case that
      // cov is true, that is the way that subtyping works on functions
      // example a function that accepts a double can be passed to a place
      // that accepts functions that accept an int
      // (the reverse relation that the arguments have).
      if (!compare_types(right_func->input(i), left_func->input(i), cov)) {
        return false;
      }
    }

    return true;
  } else if (left->name() == cdk::TYPE_POINTER && right->name() == cdk::TYPE_POINTER) {
    auto left_ref = cdk::reference_type::cast(left);
    auto right_ref = cdk::reference_type::cast(right);

    return compare_types(left_ref->referenced(), right_ref->referenced(), cov);
  } else if (cov && left->name() == cdk::TYPE_DOUBLE && right->name() == cdk::TYPE_INT) {
    // only base case where covariance changes something
    return true;
  } else {
    return left->name() == right->name();
  }
}

//---------------------------------------------------------------------------
void til::type_checker::do_declaration_node(til::declaration_node * const node, int lvl) {
  if (node->type() != nullptr) { // type defined on declaration
    if (node->initial() != nullptr) { // has initializer
      node->initial()->accept(this, lvl);

      if (node->initial()->is_typed(cdk::TYPE_UNSPEC)) {
        node->initial()->type(node->type());
      }

      if (node->is_typed(cdk::TYPE_POINTER) && node->initial()->is_typed(cdk::TYPE_POINTER)) {
        auto lvalue_pointer = cdk::reference_type::cast(node->type());
        auto rvalue_pointer = cdk::reference_type::cast(node->initial()->type());

        if (rvalue_pointer->referenced()->name() == cdk::TYPE_UNSPEC ||
            rvalue_pointer->referenced()->name() == cdk::TYPE_VOID ||
            lvalue_pointer->referenced()->name() == cdk::TYPE_VOID) {
              node->initial()->type(node->type());
            }
      }

      if (!compare_types(node->type(), node->initial()->type(), true)) {
        throw "type mismatch on declaration of" + node->identifier();
      }
    }
  } else { // type infered
    node->initial()->accept(this, lvl);

    if (node->initial()->is_typed(cdk::TYPE_UNSPEC)) {
      // if we have a variable with type inference that has an initializer
      // that needs their type defined by the parent (undefined behaviour)
      // we assign the type int
      node->initial()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }

    node->type(node->initial()->type());
  }

  auto sym = std::make_shared<til::symbol>(node->type(), node->identifier(), node->qualifier());

  if (_symtab.insert(sym->name(), sym)) {
    return; // Success
  }

  throw "redeclaring variable" + node->identifier();
}

//---------------------------------------------------------------------------
void til::type_checker::do_block_node(til::block_node * const node, int lvl) {
}

//---------------------------------------------------------------------------
void til::type_checker::do_function_call_node(til::function_call_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->args()->accept(this, lvl);
  std::shared_ptr<cdk::functional_type> func_type;

  if (node->function_pointer() == nullptr) {
    auto sym = _symtab.find("@");
    if (sym == nullptr) {
      throw "recursive function call outside function";
    }
    func_type = cdk::functional_type::cast(sym->type());
  }else {
    node->function_pointer()->accept(this, lvl);
    if (!node->function_pointer()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw "wrong type in function pointer";
    }
    func_type = cdk::functional_type::cast(node->function_pointer()->type());
  }

  if (func_type->input_length() != node->args()->size()) {
      throw "wrong number of arguments in recursive function call";
  }

  for (size_t i = 0; i < node->args()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->args()->node(i));

    if (arg->is_typed(cdk::TYPE_UNSPEC)) {
      arg->type(func_type->input(i));
    }

    if (!compare_types(func_type->input(i), arg->type(), true)) {
      throw "wrong type in argument of recursive function call";
    }
  }

  node->type(func_type->output(0));
}

//---------------------------------------------------------------------------
void til::type_checker::do_objects_operator_node(til::objects_operator_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------
void til::type_checker::do_sizeof_operator_node(til::sizeof_operator_node * const node, int lvl) {
  node->argument()->accept(this, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------
void til::type_checker::do_referencing_operator_node(til::referencing_operator_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->lval()->accept(this, lvl);

  node->type(cdk::reference_type::create(4, node->lval()->type()));
}

//---------------------------------------------------------------------------
void til::type_checker::do_pointer_indexing_node(til::pointer_indexing_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->pointer()->accept(this, lvl);
  node->index()->accept(this, lvl);

  if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
    node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (!node->index()->is_typed(cdk::TYPE_INT)) {
    throw std::string("index is non-integer");
  }

  if (!node->pointer()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("trying to index a non-pointer");
  }

  auto type = cdk::reference_type::cast(node->pointer()->type());

  node->type(type->referenced());
}