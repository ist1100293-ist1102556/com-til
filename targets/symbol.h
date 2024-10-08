#ifndef __TIL_TARGETS_SYMBOL_H__
#define __TIL_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace til {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    ssize_t _offset = 0; // Since a real offset cannot be 0, if a symbol 
                     // has offset 0, it means that it is a global variable.
    int _qualifier = 0;

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, int qualifier) :
        _type(type), _name(name), _qualifier(qualifier) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    ssize_t offset() {
      return _offset;
    }
    void offset(ssize_t offset) {
      _offset = offset;
    }
    int qualifier() {
      return _qualifier;
    }
    void qualifier(int qualifier) {
      _qualifier = qualifier;
    }
  };

} // til

#endif
