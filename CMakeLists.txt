set (SOURCES src)

  set(BUILD_SHARED_LIBS ON)

include_directories( src)

add_custom_command(OUTPUT src/parser.c src/node.types src/tree-sitter.h
  COMMAND tree-sitter generate
  DEPENDS grammar.js  )

cmake_minimum_required(VERSION 3.26)


project(tree-sitter-prolog)


#talk to python.pl
add_library(tree-sitter-yap  src/parser.c)

set(INSTALL_LIBDIR $ENV{HOME}/.emacs.d/tree-sitter)

    install(TARGETS  tree-sitter-yap
      LIBRARY DESTINATION ${INSTALL_LIBDIR}
      RUNTIME DESTINATION ${INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${INSTALL_LIBDIR} )

# arithmetic hassle.      
# ;;target_link_libraries(Py4YAP  libYap  ${Python3_LIBRARIES})




