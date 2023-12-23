CODEIN := input.a
CODEOUT := output.asm

INCDIR := inc
SRCDIR := src
PROJECT := compiler.exe

CPP := g++
CPPFLAGS := -Wall -g -MMD -c -I inc -std=c++20 -fno-rtti -fno-exceptions

SOURCES := $(wildcard *.cpp) $(wildcard $(SRCDIR)/*.cpp) $(wildcard $(SRCDIR)/*/*.cpp)
OBJECTS := $(SOURCES:%.cpp=%.o)
DEPENDENCIES = $(OBJECTS:%.o=%.d)

run: $(PROJECT)
	./$(PROJECT) $(CODEIN) $(CODEOUT)

$(PROJECT): $(OBJECTS)
	$(CPP) -o $@ $^

%.o: %.cpp
	$(CPP) $(CPPFLAGS) -o $@ $<

.PHONY: clean
clean:
	$(shell rm -f $(OBJECTS) $(DEPENDENCIES))

-include $(OBJECTS:%.o=%.d)