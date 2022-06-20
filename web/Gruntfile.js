module.exports = function(grunt) {
  grunt.loadNpmTasks("grunt-terser");
  grunt.loadNpmTasks("grunt-contrib-htmlmin");
  grunt.loadNpmTasks("grunt-contrib-cssmin");

  grunt.initConfig({
    pkg: grunt.file.readJSON("package.json"),
    terser: {
      options: {
        mangle: false,
        keep_fnames: true,
        timings: true,
      },
      build: {
        files: [{
          expand: true,
          flatten: true,
          cwd: ".",
          src: ["temp/js/*.js"],
          dest: "min/js",
        }],
      },
    },
    htmlmin: {
      dist: {
        options: {
          removeComments: true,
          collapseWhitespace: true,
        },
        files: [{
          expand: true,
          flatten: true,
          cwd: ".",
          src: ["temp/html/*.html"],
          dest: "min/html",
        }],
      },
    },
    cssmin: {
      target: {
        files: [{
          expand: true,
          flatten: true,
          cwd: ".",
          src: ["src/css/*.css"],
          dest: "min/css",
        }],
      },
    },
  });

  grunt.registerTask("default", ["terser", "htmlmin", "cssmin"]);
};
