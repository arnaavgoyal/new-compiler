CPP := g++
CPPFLAGS := -Wall -g -MMD -c
SOURCES = $(wildcard *.cpp) $(wildcard */*.cpp)
OBJECTS = $(SOURCES:%.cpp=%.o)
DEPENDENCIES = $(OBJECTS:%.o=%.d)
PROJECT := compiler.exe

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