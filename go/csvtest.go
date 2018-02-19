package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"os"
)

func processFile(file *os.File, keyField, valueField int) {
	var sumByKey = make(map[string]int)
	maxField := keyField
	if valueField > maxField {
		maxField = valueField
	}

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Bytes()
		key, val := getKeyVal(line, keyField, valueField, maxField)
		sumByKey[key] += val
	}
	var k string
	v := -1
	for key, val := range sumByKey {
		if val > v {
			k = key
			v = val
		}
	}
	fmt.Printf("max_key: %s sum: %d", k, v)

}

func main() {

	file, _ := os.Open(os.Args[1])
	defer file.Close()

	keyField, _ := atoi([]byte(os.Args[2]))
	valueField, _ := atoi([]byte(os.Args[3]))
	processFile(file, keyField, valueField)

}

func getKeyVal(line []byte, keyField, valueField, maxField int) (string, int) {
	var i int
	var tabIndex int
	var k []byte
	var v []byte
	var field []byte

	for i <= maxField && len(line) > 0 {
		tabIndex = bytes.IndexByte(line, '\t') // returns -1 if not found
		if tabIndex < 0 {
			field = line
			line = nil
		} else {
			field = line[:tabIndex]
			line = line[tabIndex+1:]
		}
		switch i {
		case keyField:
			k = field
		case valueField:
			v = field
		}
		i++
	}
	val, _ := atoi(v)
	return string(k), val
}

var errAtoi = errors.New("invalid number")

func atoi(input []byte) (int, error) {
	val := 0
	for i := 0; i < len(input); i++ {
		char := input[i]
		if char < '0' || char > '9' {
			return 0, errAtoi
		}
		val = val*10 + int(char) - '0'
	}
	return val, nil
}
