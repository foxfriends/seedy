package config

import (
	"cuelang.org/go/cue/cuecontext"
	"os"
)

type Config struct {
	SeedsDir string
}

func Default() *Config {
	return &Config{
		SeedsDir: "seeds",
	}
}

type configError string
func (self configError) Error() string { return string(self) }

func Load(path string) (*Config, error) {
	ctx := cuecontext.New()
	paths := []string{
		"seedy.cue",
		"seedy.json",
	}
	if len(path) != 0 {
		paths = []string{path}
	}

	for _, path := range paths {
		if _, err := os.Stat(path); err != nil {
			if os.IsNotExist(err) {
				continue
			}
			return nil, err
		}

		source, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}

		value := ctx.CompileBytes(source)
		config := Default()
		value.Decode(config)
		if value.Err() != nil {
			return nil, value.Err()
		}
		return config, nil
	}

	return nil, configError("seedy configuration could not be found in any of the default locations (seedy.cue, seedy.json), run `seedy init` to generate a default configuration")
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
