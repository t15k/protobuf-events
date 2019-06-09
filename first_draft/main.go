package protobufevent

import (
	"bytes"
	"encoding/binary"
	"io"
)

// Handler takes a reader and will unmarshal to type
// handled by the handler, it will consume bytes from the
// as a side effect of this.
type Handler interface {
	Handle(r io.ByteReader) error
}

type MessageHandler struct {
	fieldHandlers map[uint64]Handler
}
type MessageHandler2 map[uint64]Handler

func (h MessageHandler2) Handle(r io.ByteReader) error {
	field, err := binary.ReadUvarint(r)
	if err != nil {
		return err
	}
	fieldType := field & 0x07
	switch fieldType {
	case 0x01:
		if fh, ok := h[field>>3]; ok {
			if err := fh.Handle(r); err != nil {
				return err
			}
		}
		return nil
	case 0x02:
		panic("not supported")
	case 0x03:
		panic("not supported")
	case 0x04:
		panic("not supported")
	case 0x05:
		panic("not supported")
	}
	return nil
}

// ParseBytes will parse the bytes with the current handlers
// registered on this MessageHandler.
func (h *MessageHandler2) ParseBytes(b []byte) error {
	buf := bytes.NewBuffer(b)
	return h.Handle(buf)
}

type int32Handler struct{}

type stringHandler struct{}

// ParseBytes will parse the bytes with the current handlers
// registered on this MessageHandler.
func (h *MessageHandler) ParseBytes(b []byte) error {
	buf := bytes.NewBuffer(b)
	return h.Handle(buf)
}

// AddInt64Handler will register the given handler under the given nummeric
// field ID.
func (h *MessageHandler) AddInt64Handler(fieldID uint64, handler Handler) {
	if h.fieldHandlers == nil {
		h.fieldHandlers = make(map[uint64]Handler)
	}
	h.fieldHandlers[fieldID] = handler
}

// Handle parses the given bytes, using t as
// for delegating found fields
func (h *MessageHandler) Handle(r io.ByteReader) error {
	field, err := binary.ReadUvarint(r)
	if err != nil {
		return err
	}
	fieldType := field & 0x07
	switch fieldType {
	case 0x01:
		if fh, ok := h.fieldHandlers[field>>3]; ok {
			if err := fh.Handle(r); err != nil {
				return err
			}
		}
		return nil
	case 0x02:
		panic("not supported")
	case 0x03:
		panic("not supported")
	case 0x04:
		panic("not supported")
	case 0x05:
		panic("not supported")
	}
	return nil
}

type uint64Handler struct {
	callback func(v uint64)
}

func (h uint64Handler) Handle(r io.ByteReader) (err error) {
	var v uint64
	if v, err = binary.ReadUvarint(r); err == nil {
		if h.callback != nil {
			h.callback(v)
		}
	}
	return
}

type int64Handler struct {
	callback func(v int64)
}

type uint32Handler struct {
	callback func(v uint32)
}
type boolHandler struct {
	callback func(v bool)
}
type floatHandler struct {
	callback func(v float32)
}
type doubleHandler struct {
	callback func(v float64)
}
type binaryHandler struct {
	callback func(v []byte)
}

// Handlers is a collection of handlers organised by
// fields numbers
type Handlers struct {
	iHandlers map[uint64]func(io.ByteReader) error
	sHandlers map[uint64]func(string)
	nHandlers map[uint64]func(*Handlers)
}

/*
	typ := b[0] & 0x07
*/
