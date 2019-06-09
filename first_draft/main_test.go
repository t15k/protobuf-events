package protobufevent

import (
	"testing"
)

func TestParseBytes(t *testing.T) {
	var h MessageHandler
	var i uint64
	h.AddInt64Handler(1, uint64Handler{func(v uint64) {
		i = v
	}})

	if err := h.ParseBytes([]byte{9, 5}); err != nil {
		t.Error(err)
	}
	if i != 5 {
		t.Errorf("got %d extepcted 5", i)
	}
}

func TestRegisterFieldPathHandlers(t *testing.T) {
	var i uint64
	h := MessageHandler2{
		1: uint64Handler{func(v uint64) {
			i = v
		}},
	}
	if err := h.ParseBytes([]byte{9, 5}); err != nil {
		t.Error(err)
	}
	if i != 5 {
		t.Errorf("got %d extepcted 5", i)
	}
}
