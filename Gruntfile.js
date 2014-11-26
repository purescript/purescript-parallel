module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],
    
    clean: ["tmp", "output"],
  
    pscDocs: {
      lib: {
        src: ["src/**/*.purs"],
        dest: "docs/README.md"
      }
    },

    psc: {
      options: {
        modules: ["Main"],
        main: "Main"
      },
      example: {
        src: ["<%=libFiles%>", "example/Main.purs"],
        dest: "tmp/Main.js"
      }
    },
    
    pscMake: {
      lib: {
        src: ["<%=libFiles%>"]
      },
      tests: {
        src: ["tests/Tests.purs", "<%=libFiles%>"]
      }
    },

    dotPsci: ["<%=libFiles%>"]
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("example", ["psc:example"]);
  grunt.registerTask("make", ["pscMake:lib", "pscDocs:lib", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make"]);
};
