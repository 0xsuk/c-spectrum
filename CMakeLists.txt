cmake_minimum_required(VERSION 3.10)

project(c-spectrum)

# does not work
# set(CMAKE_GENERATOR "MSYS Makefiles")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_COMPILER cc)
set(CMAKE_C_FLAGS "-Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I C:/msys64/mingw64/include/") #so that clangd can find default includes

add_executable(${PROJECT_NAME} WIN32 main.c)

find_package(SDL2 REQUIRED)
find_package(SDL2 REQUIRED SDL2main)
find_package(SDL2_image REQUIRED)
find_package(portaudio REQUIRED)

target_link_libraries(${PROJECT_NAME} mingw32 SDL2main SDL2 SDL2_image kissfft user32 shell32 portaudio)
