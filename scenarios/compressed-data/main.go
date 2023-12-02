package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

var (
	formatFlag = flag.String("format", "lz4", "the compression format")
)

const N = 1000
const P = 1

type person struct {
	Name string
	Age  int
}

func main() {
	flag.Parse()
	var topic, codec string
	switch *formatFlag {
	case "lz4":
		topic = "lz4-data"
		codec = "lz4"
	case "snappy":
		topic = "snappy-data"
		codec = "snappy"
	case "zstandard":
		topic = "zstd-data"
		codec = "zstd"
	default:
		log.Fatalf("error: unsupported --format: %s", *formatFlag)
	}

	p, err := kafka.NewProducer(&kafka.ConfigMap{
		"bootstrap.servers": "127.0.0.1",
		"compression.codec": codec,
	})
	if err != nil {
		panic(err)
	}
	defer p.Close()

	go func() {
		for e := range p.Events() {
			switch ev := e.(type) {
			case *kafka.Message:
				if ev.TopicPartition.Error != nil {
					fmt.Printf("Error: %v\n", ev.TopicPartition.Error)
				}
			}
		}
	}()

	value, err := json.Marshal(person{"Bogdan", 30})
	if err != nil {
		panic(err)
	}

	for i := 0; i < N; i++ {
		p.Produce(&kafka.Message{
			TopicPartition: kafka.TopicPartition{
				Topic:     &topic,
				Partition: 0,
			},
			Value: value,
		}, nil)
	}

	p.Flush(60000)
}
