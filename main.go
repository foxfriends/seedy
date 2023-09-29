package main

import (
	"fmt"
	"github.com/foxfriends/seedy/seedy/config"
	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"github.com/urfave/cli/v2"
	"log"
	"os"
)

type SeedyError string

func (self SeedyError) Error() string { return string(self) }

func getConfig(ctx *cli.Context) (*config.Config, error) {
	path := ctx.String("config")
	if path == "" {
		return config.Load()
	}
	return config.LoadFrom(path)
}

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
					if path, err := config.Find([]string{"seedy.cue", "seedy.json"}); err == nil {
						return SeedyError(fmt.Sprintf("It appears a seedy project already exists at %s, remove it before running `init` again", path))
					}
					config.Default().Save("seedy.cue")
					return nil
				},
			},
			{
				Name:  "apply",
				Usage: "update the database to the latest state",
				Action: func(ctx *cli.Context) error {
					if _, err := getConfig(ctx); err != nil {
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
					if _, err := getConfig(ctx); err != nil {
						return err
					}
					fmt.Println("todo: implement plan")
					return nil
				},
			},
			{
				Name:  "pull",
				Usage: "pull type definitions for seeds from a live database",
				Action: func(ctx *cli.Context) error {
					config, err := getConfig(ctx)
					if err != nil {
						return err
					}
					if config.DatabaseUrl == "" {
						return SeedyError("DatabaseUrl must be set")
					}
					if _, err := sqlx.Connect("postgres", config.DatabaseUrl); err != nil {
						return err
					}
					return nil
				},
			},
		},
	}
	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}
