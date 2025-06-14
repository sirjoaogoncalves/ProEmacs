# ProEmacs: A Modular, Performance-Focused Emacs Configuration

ProEmacs is a highly modular, performance-optimized Emacs configuration designed for modern development workflows. Built with a focus on speed, usability, and extensibility, it provides a comprehensive set of features while maintaining fast startup times and responsive editing.

## Features

### Core Features

* **Modular Architecture**: Cleanly separated configuration files for better organization and maintenance
* **Evil Mode Integration**: Vim keybindings with extensive customization and proper evil-collection setup
* **Performance Optimizations**: GC tuning, lazy loading, native compilation support
* **Modern UI**: Clean, distraction-free interface with Doom themes and modeline
* **Keybinding System**: Intuitive, spacemacs-like keybindings with which-key integration

### 🤖 AI Coding Assistant
- **Multi-provider support**: Local Ollama, Groq, OpenAI, Claude, Gemini
- **Code explanation and documentation generation**
- **Intelligent refactoring suggestions**
- **Real-time code reviews and analysis**
- **Interactive coding questions and answers**
- **Local AI with Ollama for privacy and unlimited usage**

### Development Tools

* **LSP Support**: Integrated Language Server Protocol for intelligent code assistance
* **Syntax Checking**: On-the-fly error detection with flycheck
* **Version Control**: Comprehensive Git integration via Magit
* **Project Management**: Seamless project navigation with Projectile
* **Code Completion**: Fast, context-aware completion with Corfu and Cape
* **AI Assistance**: Integration with Minuet for AI code completion
* **Tree-sitter Support**: Enhanced syntax highlighting for Emacs 29+

### 🐳 Docker Integration
- **Container management directly from Emacs**
- **Dockerfile editing with intelligent completion**
- **Docker Compose support and operations**
- **Real-time container monitoring and logs**
- **Integrated terminal for Docker commands**

### 🔍 Advanced Search
- **Enhanced file search with Consult and Vertico**
- **Project-wide search with ripgrep integration**
- **Intelligent completion with Orderless matching**
- **Search result filtering and context-aware suggestions**
- **Multi-directory and multi-format file support**

## Installation

### Prerequisites
- **Emacs 29.1+** (recommended)
- **Git** for cloning repositories
- **Node.js** (optional, for some features)

### Quick Setup
```bash
# Clone the configuration
git clone <your-repo> ~/.emacs.d

# Start Emacs (it will automatically install packages)
emacs
```

## AI Coding Assistant Setup

### Local AI with Ollama (Recommended)
```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Start Ollama server
ollama serve

# Pull a coding model
ollama pull deepseek-coder:latest
# or for better performance (if you have enough RAM):
ollama pull deepseek-coder:33b
```

**Configuration in `init.el`:**
```elisp
;; AI provider configuration
(with-eval-after-load 'ai-enhanced-coding
  (setq ai-coding-default-provider 'local))
```

### Cloud AI Providers

#### Groq (Fast & Free)
1. **Get API key**: https://console.groq.com
2. **Add to `init.el`:**
   ```elisp
   (setenv "GROQ_API_KEY" "your_groq_key_here")
   ```

#### OpenAI (Premium)
1. **Get API key**: https://platform.openai.com
2. **Add to `init.el`:**
   ```elisp
   (setenv "OPENAI_API_KEY" "your_openai_key_here")
   ```

#### Claude (Premium)
1. **Get API key**: https://console.anthropic.com
2. **Add to `init.el`:**
   ```elisp
   (setenv "ANTHROPIC_API_KEY" "your_claude_key_here")
   ```

### Provider Management
```elisp
;; List all available providers
SPC A p l

;; Switch provider interactively
SPC A p s

;; Or set manually
(setq ai-coding-default-provider 'groq)  ; groq, local, openai, claude
```

## Docker Integration Setup

### Prerequisites
```bash
# Install Docker
curl -fsSL https://get.docker.com | sh

# Add your user to docker group (Linux)
sudo usermod -aG docker $USER
```

### Configuration
The Docker integration works out of the box. Key features include:

- **Container Management**: Start, stop, and monitor containers
- **Image Operations**: Build, pull, and manage Docker images  
- **Compose Support**: Full docker-compose integration
- **Log Viewing**: Real-time container log streaming

## Advanced Search Setup

### Prerequisites
```bash
# Install ripgrep for fast searching
# Ubuntu/Debian:
sudo apt install ripgrep

# macOS:
brew install ripgrep

# Arch Linux:
sudo pacman -S ripgrep
```

### Optional Search Tools
```bash
# For even better search performance
npm install -g @antfu/ni  # Package manager detection
pip install --user pyfzf  # Fuzzy finding enhancement
```

## Usage

ProEmacs uses a spacemacs-like leader key approach with `SPC` as the primary prefix in normal mode and `C-SPC` as a global prefix. Key bindings are organized into logical groups:

* `SPC f` - File operations
* `SPC b` - Buffer operations
* `SPC w` - Window management
* `SPC p` - Project operations
* `SPC g` - Git commands
* `SPC l` - Layouts (workspaces)
* `SPC o` - Org mode commands
* `SPC d` - Dired operations
* `SPC L` - LSP features
* `SPC A` - AI coding assistant  *(NEW)*
* `SPC D` - Docker operations   *(NEW)*
* `SPC s` - Search operations   *(Enhanced)*
* `SPC t` - Toggle options (including themes)

### 🤖 AI Coding Assistant

#### Key Bindings
| Command | Description |
|---------|-------------|
| `SPC A e` | Explain selected code |
| `SPC A r` | Refactor code |
| `SPC A d` | Generate documentation |
| `SPC A c` | Ask coding questions |
| `SPC A R` | Code review |
| `SPC A g` | Generate code from description |
| `SPC A f` | Fix code issues |
| `SPC A p l` | List available providers |
| `SPC A p s` | Switch AI provider |

#### Example Workflow
1. **Select code** you want to understand
2. **Press `SPC A e`** to get an AI explanation
3. **Press `SPC A r`** to get refactoring suggestions
4. **Press `SPC A d`** to generate documentation

#### Interactive Coding
```
SPC A c → Ask: "How do I implement a binary search in Python?"
SPC A g → Generate: "Create a REST API endpoint for user authentication"
```

### 🐳 Docker Integration

#### Key Bindings
| Command | Description |
|---------|-------------|
| `SPC D l` | List containers |
| `SPC D r` | Run container |
| `SPC D s` | Stop container |
| `SPC D b` | Build image |
| `SPC D c` | Docker Compose operations |
| `SPC D L` | View container logs |

#### Example Workflow
1. **Open a Dockerfile** in Emacs
2. **Press `SPC D b`** to build the image
3. **Press `SPC D r`** to run a container
4. **Press `SPC D L`** to view logs

### 🔍 Advanced Search

#### Key Bindings
| Command | Description |
|---------|-------------|
| `SPC s s` | Search in buffer (consult-line) |
| `SPC s p` | Search in project |
| `SPC s d` | Search in directory |
| `SPC s f` | Find file |
| `SPC s r` | Search and replace |
| `SPC c l` | Search lines with preview |
| `SPC c i` | Search imenu items |

#### Smart Search Features
- **Context-aware suggestions** with Marginalia
- **Flexible matching** with Orderless
- **Vertical completion** with Vertico
- **Multi-format file support** with enhanced backends

### Theme-Related Bindings

* `SPC t T` - Doom theme selector (quick access to popular themes)
* `SPC t t` - Load any available theme
* `SPC t l` - Toggle line numbers
* `SPC t m` - Toggle menu bar

Press `SPC` and wait for which-key to display available options.

## Available Doom Themes

ProEmacs includes a curated selection of Doom themes:

**Dark Themes:**
- `doom-one` - The classic, popular dark theme
- `doom-vibrant` - Vibrant, colorful theme
- `doom-dracula` - Dark theme with purple accents
- `doom-nord` - Cool, Nordic-inspired theme
- `doom-gruvbox` - Retro groove theme
- `doom-palenight` - Material design inspired
- `doom-monokai-pro` - Professional monokai
- `doom-city-lights` - Urban-inspired theme
- `doom-tomorrow-night` - Tomorrow night theme

**Light Themes:**
- `doom-solarized-light` - Light solarized variant

**And more!** Access the full list via `SPC t T`.

## Project Structure

```
.
├── core
│   ├── defaults.el      # Better Emacs defaults
│   ├── keybindings.el   # Global key bindings
│   ├── packages.el      # Package management
│   └── ui.el            # User interface settings (themes, modeline)
├── early-init.el        # Early initialization (pre-GUI)
├── init.el              # Main initialization file
└── modules
    ├── ai-enhanced-coding.el # AI coding assistant (NEW)
    ├── ai.el            # Minuet AI code completion
    ├── buffer-management.el # Buffer cleanup and organization
    ├── completion.el    # Completion frameworks (Enhanced)
    ├── dashboard-config.el # Startup dashboard
    ├── development.el   # Programming tools
    ├── dired-config.el  # File manager enhancements
    ├── docker-integration.el # Docker support (NEW)
    ├── evil-config.el   # Vim emulation (fixed for evil-collection)
    ├── format-utils.el  # Code formatting
    ├── git.el           # Git integration
    ├── modern-features.el # Newer Emacs features
    ├── org-config.el    # Org mode setup
    ├── performance.el   # Performance optimizations
    ├── process-manager.el # Background process management
    ├── remote-file-utils.el # Remote file operations
    ├── tabs.el          # Tab bar configuration
    ├── terminal.el      # Terminal integration
    ├── threading-utils.el # Thread pool management
    └── window-config.el # Window layout management
```

## Configuration

### AI Coding Configuration
```elisp
;; AI Coding Assistant Settings
(setq ai-coding-temperature 0.7          ; Creativity level (0.0-1.0)
      ai-coding-max-tokens 2000          ; Response length
      ai-coding-auto-explain nil         ; Auto-explain on selection
      ai-coding-show-tokens t)           ; Show token usage

;; Provider-specific settings
(setq ai-coding-ollama-host "localhost:11434"
      ai-coding-groq-model "llama3-8b-8192"
      ai-coding-openai-model "gpt-4")
```

### Minuet AI Configuration
```elisp
;; Minuet for AI code completion
(setq minuet-provider 'gemini)
(plist-put minuet-gemini-options :model "gemini-2.0-flash")
(plist-put minuet-gemini-options :thinking nil)
(setq minuet-auto-suggestion-debounce-delay 0.5)
(setq minuet-auto-suggestion-throttle-delay 1.0)
(setq minuet-n-completions 2)
```

### Docker Configuration
```elisp
;; Docker Integration Settings
(setq docker-container-shell "/bin/bash"
      docker-compose-command "docker-compose"
      docker-log-tail-lines 100)
```

### Search Configuration
```elisp
;; Advanced Search Settings with Consult and Vertico
(setq consult-narrow-key "<"
      consult-line-numbers-widen t
      consult-async-min-input 2
      consult-async-refresh-delay 0.15
      consult-async-input-throttle 0.2
      consult-async-input-debounce 0.1)
```

## Customization

ProEmacs is designed to be easily customizable:

1. **Personal Configuration**: Create a `personal.el` file in the root directory for personal settings
2. **Modifying Existing Modules**: Each module is self-contained and can be modified independently
3. **Adding New Modules**: Create new `.el` files in the `modules` directory and require them in `init.el`
4. **Custom Packages**: Add additional packages in `packages.el`
5. **Theme Customization**: Modify `core/ui.el` to set your preferred default theme

### Custom Theme Setup

To set a default theme that loads automatically:

1. Edit `core/ui.el`
2. Uncomment your preferred theme line:
   ```elisp
   (load-theme 'doom-dracula t)  ; Enable this for doom-dracula as default
   ```
3. Restart Emacs

## Performance Features

ProEmacs includes several optimizations for a responsive editing experience:

* **Garbage Collection Strategy**: Intelligent GC adjustments during startup and idle time
* **Native Compilation**: Support for Emacs' native compilation (gccemacs)
* **Lazy Loading**: Deferred loading of non-essential packages
* **Process Management**: Automatic suspension of idle background processes
* **LSP Optimizations**: Tuned settings for responsive LSP operation
* **Thread Pool**: Background thread utilization for heavy operations (Emacs 28+)
* **Font Optimization**: Safe font loading with compatibility checks

## Troubleshooting

### Common Issues

1. **Missing Icons**: Run `M-x all-the-icons-install-fonts`
2. **Font Errors**: The configuration safely handles missing fonts
3. **Theme Not Loading**: Use `SPC t T` to select a theme interactively
4. **Package Errors**: Delete `~/.emacs.d/elpa` and restart Emacs to reinstall packages

### AI Issues

#### "No providers available"
- **Check API keys** are set in environment variables
- **Restart Emacs** after setting keys
- **Verify network connection** for cloud providers

#### "Ollama connection failed"
```bash
# Check if Ollama is running
ps aux | grep ollama

# Start Ollama if not running
ollama serve

# Test API connection
curl http://localhost:11434/api/tags
```

#### "Model not found"
```bash
# List available models
ollama ls

# Pull missing model
ollama pull deepseek-coder:latest
```

### Docker Issues

#### "Docker daemon not running"
```bash
# Start Docker service (Linux)
sudo systemctl start docker

# Start Docker Desktop (macOS/Windows)
```

#### "Permission denied"
```bash
# Add user to docker group (Linux)
sudo usermod -aG docker $USER
# Then logout and login again
```

### Search Issues

#### "Ripgrep not found"
```bash
# Install ripgrep
# Ubuntu/Debian:
sudo apt install ripgrep

# macOS:
brew install ripgrep
```

### Evil Collection Warnings

If you see warnings about `evil-want-keybinding`, the configuration has been fixed to properly set this variable before loading evil-collection.

### Performance Issues

* Use `SPC P` for performance tools and monitoring
* Check `SPC P t` for package load times
* Use `SPC P g` to manually trigger garbage collection

## Recent Changes

* **NEW: AI Coding Assistant**: Complete AI integration with multiple providers including local Ollama support
* **NEW: Docker Integration**: Full Docker container and image management from within Emacs
* **Enhanced Search**: Upgraded search system with Consult, Vertico, and Orderless for better fuzzy finding
* **Fixed Evil Collection Integration**: Properly configured `evil-want-keybinding` to eliminate warnings
* **Added Doom Themes**: Complete doom-themes package with 15+ theme variants
* **Added Doom Modeline**: Modern, informative modeline with icons
* **Enhanced Theme System**: Quick theme selector accessible via `SPC t T`
* **Improved Package Management**: More robust package initialization and error handling
* **Enhanced Font Handling**: Safe font loading with system compatibility checks

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- **Doom Emacs** for the excellent foundation and inspiration
- **Ollama** for local AI capabilities
- **Docker** for containerization
- **Ripgrep** for fast searching
- **All contributors** who made this possible

---

**ProEmacs** - A modern, fast, and beautiful Emacs configuration for developers who want power without the complexity.
