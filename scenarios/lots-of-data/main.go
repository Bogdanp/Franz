package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"time"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

var (
	numDays       = flag.Int("num-days", 30, "the number of days to generate data for")
	numCandles    = flag.Int("num-candles-per-day", 1*1000*1000, "the number of candles to generate per day")
	numPartitions = flag.Int("num-partitions", 1, "the number of partitions to publish to")
)

type candle struct {
	Open  float64
	High  float64
	Low   float64
	Close float64
}

func main() {
	flag.Parse()

	p, err := kafka.NewProducer(&kafka.ConfigMap{
		"bootstrap.servers":      "127.0.0.1",
		"queue.buffering.max.ms": 60000,
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

	topic := "lots-of-data"
	st := time.Now().Truncate(24 * time.Hour).Add(-time.Duration(*numDays) * time.Hour * 24)
	ival := (24 * time.Hour) / time.Duration(*numCandles)
	log.Println(ival)
	for i := 0; i < *numDays; i++ {
		for part := 0; part < *numPartitions; part++ {
			ts := st
			log.Println(ts, part)
			for j := 0; j < *numCandles; j++ {
				k := ts.Format(time.RFC3339Nano)
				v, _ := json.Marshal(candle{rand.Float64(), rand.Float64(), rand.Float64(), rand.Float64()})

				for {
					err := p.Produce(&kafka.Message{
						TopicPartition: kafka.TopicPartition{
							Topic:     &topic,
							Partition: int32(part),
						},
						Key:       []byte(k),
						Value:     v,
						Timestamp: ts,
					}, nil)
					if err == nil {
						break
					} else if err.(kafka.Error).Code() == kafka.ErrQueueFull {
						log.Println("queue full")
						p.Flush(1000)
						continue
					} else if err != nil {
						panic(err)
					}
				}
				ts = ts.Add(ival)
			}
			p.Flush(1000)
		}
		p.Flush(1000)
		st = st.Add(time.Hour * 24)
	}
	p.Flush(1000)
}
