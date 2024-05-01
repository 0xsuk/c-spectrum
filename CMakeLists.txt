cmake_minimum_required(VERSION 3.10)


project(cl-spectrum)

# does not work
# set(CMAKE_GENERATOR "MSYS Makefiles")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_COMPILER cc)
set(CMAKE_C_FLAGS "-Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I C:/msys64/mingw64/include/") #so that clangd can find default includes
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I ${CMAKE_CURRENT_SOURCE_DIR}/SDL2-2.30.2/include")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I ${CMAKE_CURRENT_SOURCE_DIR}/kissfft")

# must be called before add_executable???
link_directories(
  "${CMAKE_CURRENT_SOURCE_DIR}/SDL2-2.30.2/lib/x64"
  "${CMAKE_CURRENT_SOURCE_DIR}/kissfft/build"
)

add_executable(${PROJECT_NAME} main.c)


target_link_libraries(${PROJECT_NAME} SDL2main SDL2 kissfft-float user32 shell32 portaudio)
