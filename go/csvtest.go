package main

import (
	"bufio"
	"bytes"
	"errors"
	"log"
	"os"
)

func processLine(b []byte) (int, int) {
	key := 0
	val := 0
	i := 0
	for b[i] != '\t' {
		i++
	}
	for i++; b[i] != '\t'; i++ {
		key = key*10 + int(b[i]) - '0'
	}
	for i++; b[i] != '\t'; i++ {
		val = val*10 + int(b[i]) - '0'
	}
	return key, val
}

func processFile(file *os.File) (int, int) {
	var sumByKey [2009]int

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Bytes()
		k1, v1 := processLine(line)
		sumByKey[k1] += v1
	}
	var k int
	var v int
	for i, val := range sumByKey {
		if val > v {
			k = i
			v = val
		}
	}
	return k, v

}

func main() {

	file, err := os.Open(os.Args[1])
	defer file.Close()
	if err != nil {
		log.Fatal(err)
	}
	processFile(file)
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
	val := atoi(v)
	return string(k), val
}

var errAtoi = errors.New("invalid number")

func atoi(s []byte) int {
	i := 0
	x := 0
	for ; i < len(s); i++ {
		c := s[i]
		x = x*10 + int(c) - '0'
	}
	return x
}
