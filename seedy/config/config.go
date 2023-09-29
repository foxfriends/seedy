package config

import (
	"cuelang.org/go/cue/cuecontext"
	"os"
)

type Config struct {
	SeedsDir    string
	DatabaseUrl string
}

func fallback(val string, fallback string) string {
	if val == "" {
		return fallback
	}
	return val
}

func Default() *Config {
	return &Config{
		SeedsDir:    fallback(os.Getenv("SEEDY_SEEDS_DIR"), "./seeds"),
		DatabaseUrl: fallback(os.Getenv("SEEDY_DATABASE_URL"), os.Getenv("DATABASE_URL")),
	}
}

type configError string

func (self configError) Error() string { return string(self) }

func Load() (*Config, error) {
	path, err := Find([]string{
		"seedy.cue",
		"seedy.json",
	})
	if err != nil {
		return nil, err
	}
	return LoadFrom(path)
}

func LoadFrom(path string) (*Config, error) {
	source, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	ctx := cuecontext.New()
	value := ctx.CompileBytes(source)
	config := Default()
	value.Decode(config)
	if value.Err() != nil {
		return nil, value.Err()
	}
	return config, nil
}

func Find(paths []string) (string, error) {
	for _, path := range paths {
		if _, err := os.Stat(path); err != nil {
			if os.IsNotExist(err) {
				continue
			}
			return "", err
		}
		return path, nil
	}

	return "", configError("seedy configuration could not be found in any of the default locations (seedy.cue, seedy.json), run `seedy init` to generate a default configuration")
}

func (self *Config) Save(path string) error {
	ctx := cuecontext.New()
	value := ctx.Encode(self)
	json, err := value.MarshalJSON()
	if err != nil {
		return err
	}
	return os.WriteFile(path, json, 0666)
}
