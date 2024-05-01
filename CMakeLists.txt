cmake_minimum_required(VERSION 3.10)


project(cl-spectrum)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_COMPILER cc)

set(CMAKE_C_FLAGS "-Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I ${CMAKE_CURRENT_SOURCE_DIR}/SDL2-2.30.2/include")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I ${CMAKE_CURRENT_SOURCE_DIR}/kissfft")

add_executable(${PROJECT_NAME} main.c)

target_link_directories(${PROJECT_NAME}
  "${CMAKE_CURRENT_SOURCE_DIR}/SDL2-2.30.2/lib/x64"
  "${CMAKE_CURRENT_SOURCE_DIR}/kissfft/build"
)


target_link_libraries(${PROJECT_NAME} SDL2main SDL2 kissfft-float user32 shell32)
