FLAGS = /Wall /I .\SDL2-2.30.2\include /I .\kissfft
LINKS = SDL2main.lib SDL2.lib user32.lib shell32.lib kissfft-float.lib /link /LIBPATH:"SDL2-2.30.2\lib\x64" /LIBPATH:"kissfft\build\Debug" /SUBSYSTEM:CONSOLE
# for production, change CONSOLE to WINDOWS
SRC = main.c


all: $(SRC)
	cl $(FLAGS) $^ $(LINKS)
