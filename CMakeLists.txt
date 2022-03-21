
cmake_minimum_required(VERSION 3.18)
project(StrictLLVM)
set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

llvm_map_components_to_libnames(REQUIRED_LLVM_LIBRARIES core scalaropts linker)

file(GLOB_RECURSE STRICTLLVM_SOURCE_FILES source/*.cpp)

add_executable(strictllvm ${STRICTLLVM_SOURCE_FILES})
target_include_directories(strictllvm PUBLIC ${LLVM_INCLUDE_DIRS} json/include)
target_link_libraries(strictllvm ${REQUIRED_LLVM_LIBRARIES})
