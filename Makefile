INCDIR := inc
SRCDIR := src
PROJECT := compiler.exe

CPP := g++
CPPFLAGS := -Wall -g -MMD -c -I inc

SOURCES := $(wildcard *.cpp) $(wildcard $(SRCDIR)/*.cpp) $(wildcard $(SRCDIR)/*/*.cpp)
OBJECTS := $(SOURCES:%.cpp=%.o)
DEPENDENCIES = $(OBJECTS:%.o=%.d)

run: $(PROJECT)
	./$(PROJECT)

$(PROJECT): $(OBJECTS)
	$(CPP) -o $@ $^

%.o: %.cpp
	$(CPP) $(CPPFLAGS) -o $@ $<

.PHONY: clean
clean:
	$(shell rm -f $(OBJECTS) $(DEPENDENCIES))

-include $(OBJECTS:%.o=%.d)