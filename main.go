package main

import (
	"fmt"
	"github.com/foxfriends/seedy/seedy/config"
	"github.com/urfave/cli/v2"
	"log"
	"os"
)

func main() {
	app := cli.App{
		Usage: "declaratively manage database seed data in code",
		Flags: []cli.Flag{
			&cli.StringFlag{
				Name:    "config",
				Aliases: []string{"c"},
				Usage:   "path to configuration file",
			},
		},
		Commands: []*cli.Command{
			{
				Name:  "init",
				Usage: "initialize a new seedy project",
				Action: func(ctx *cli.Context) error {
					config.Default().Save("seedy.cue")
					return nil
				},
			},
			{
				Name:  "apply",
				Usage: "update the database to the latest state",
				Action: func(ctx *cli.Context) error {
					if _, err := config.Load(ctx.String("config")); err != nil {
						return err
					}
					fmt.Println("todo: implement apply")
					return nil
				},
			},
			{
				Name:  "plan",
				Usage: "determine what will need to be done to get the database up to date, but not do it",
				Action: func(ctx *cli.Context) error {
					fmt.Println("todo: implement plan")
					return nil
				},
			},
		},
	}
	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}
