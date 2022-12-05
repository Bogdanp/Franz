package main

import (
	"encoding/json"
	"fmt"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

const N = 1000
const P = 1

type person struct {
	Name string
	Age  int
}

func main() {
	p, err := kafka.NewProducer(&kafka.ConfigMap{
		"bootstrap.servers": "127.0.0.1",
		"compression.codec": "lz4",
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

	topic := "lz4-data"
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
