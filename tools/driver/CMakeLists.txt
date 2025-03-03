
set(LLVM_LINK_COMPONENTS Support)

add_llvm_executable(tinyll
Tinyll.cpp)

target_link_libraries(tinyll
PRIVATE
tinylangBasic
tinylangLexer
tinylangParser
tinylangSema)
