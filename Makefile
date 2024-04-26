FLAGS = /Wall /I .\SDL2-2.30.2\include /I .\kissfft
LINKS = SDL2main.lib SDL2.lib shell32.lib kissfft-float.lib /link /LIBPATH:"SDL2-2.30.2\lib\x64" /LIBPATH:"kissfft\build\Debug" /SUBSYSTEM:WINDOWS 
SRC = main.c


all: $(SRC)
	cl $(FLAGS) $^ $(LINKS)
