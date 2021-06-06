cmake_minimum_required(VERSION 3.7)
project("fquatlib" LANGUAGES "Fortran")

#add_executable("fquatlib" "src/qdot.f95" "src/qcheck.f95")
file(GLOB_RECURSE sources src/*.f95)
add_executable(fquatlib ${sources})
