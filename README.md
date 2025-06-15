# ProEmacs: A Modular, Performance-Focused Emacs Configuration

ProEmacs is a highly modular, performance-optimized Emacs configuration designed for modern development workflows. Built with a focus on speed, usability, and extensibility, it provides a comprehensive set of features while maintaining fast startup times and responsive editing.

## Features

### Core Features

* **Modular Architecture**: Cleanly separated configuration files for better organization and maintenance
* **Evil Mode Integration**: Vim keybindings with extensive customization and proper evil-collection setup
* **Performance Optimizations**: GC tuning, lazy loading, native compilation support
* **Modern UI**: Clean, distraction-free interface with Doom themes and modeline
* **Keybinding System**: Intuitive, spacemacs-like keybindings with which-key integration

### 🤖 AI Coding Assistant (Enhanced)
- **Native Thinking Mode Support**: Toggle between quick responses and detailed reasoning chains
- **Advanced Code Analysis**: Deep explanations with step-by-step reasoning when enabled
- **Intelligent Refactoring**: Suggestions with functionality preservation guarantees  
- **Interactive AI Chat**: Multi-session conversation support with context awareness
- **Unicode Character Cleaning**: Automatic removal of problematic control characters
- **Optimized for Coding**: Uses DeepSeek-R1 for superior code understanding and generation
- **Local AI with Ollama**: Privacy-focused, unlimited usage with local model execution

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
git clone https://github.com/sirjoaogoncalves/ProEmacs ~/.emacs.d

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

# Pull the recommended reasoning model (best for coding)
ollama pull deepseek-r1:8b-0528-qwen3-q4_K_M

# Alternative models:
# ollama pull deepseek-coder-v2:latest  # Specialized for coding
# ollama pull cogito:latest             # Cutting-edge reasoning
# ollama pull phi4:latest               # Microsoft's latest model
```

**Configuration in your Emacs config:**
```elisp
;; AI enhanced coding configuration
(with-eval-after-load 'ai-enhanced-coding
  ;; Model settings
  (setq ai-coding-model "deepseek-r1:8b-0528-qwen3-q4_K_M")
  (setq ai-coding-ollama-host "localhost:11434")
  
  ;; Temperature settings
  (setq ai-coding-temperature 0.3)              ; Quick mode
  (setq ai-coding-thinking-temperature 0.6)     ; Thinking mode
  
  ;; Enable thinking mode by default for complex analysis
  (setq ai-coding-thinking-mode t))
```

### Legacy Multi-Provider Support
*Note: The enhanced AI assistant now focuses on Ollama for optimal thinking mode support. For cloud providers, consider using the Minuet integration.*

#### Minuet AI Code Completion
For additional AI providers (Groq, OpenAI, Claude, Gemini), use Minuet:

```elisp
;; Minuet configuration for cloud AI completion
(setq minuet-provider 'gemini)
(plist-put minuet-gemini-options :model "gemini-2.0-flash")
(setenv "GEMINI_API_KEY" "your_gemini_key_here")
```

## Docker Integration Setup

### Prerequisites
```bash
# Install Docker
curl -fsSL https://get.docker.com | sh

# Add  user to docker group (Linux)
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
* `SPC a` - AI coding assistant  *(Enhanced)*
* `SPC D` - Docker operations   
* `SPC s` - Search operations   *(Enhanced)*
* `SPC t` - Toggle options (including themes)

### 🤖 AI Coding Assistant (Enhanced)

#### Key Bindings
| Command | Description |
|---------|-------------|
| `SPC a e` | Explain selected code with detailed analysis |
| `SPC a r` | Get intelligent refactoring suggestions |
| `SPC a c` | Start/continue interactive AI chat session |
| `SPC a t` | Toggle thinking mode (quick ↔ detailed reasoning) |
| `SPC a s` | Start new chat session |

#### Thinking Modes

**Quick Mode** (`SPC a t` to toggle off):
- Fast, direct responses for simple questions
- Ideal for quick explanations and basic assistance
- Lower computational overhead

**Thinking Mode** (`SPC a t` to toggle on):
- Shows detailed reasoning process step-by-step
- Better for complex problems and learning
- Provides comprehensive analysis with justifications

#### Example Workflows

**Code Analysis with Thinking Mode:**
1. **Select complex code** you want to understand
2. **Enable thinking mode** with `SPC a t` (if not already on)
3. **Press `SPC a e`** to get detailed explanation with reasoning chain
4. **View both thinking process and final explanation** in results buffer

**Interactive Coding Session:**
1. **Press `SPC a c`** to open AI chat
2. **Ask coding questions** naturally: "How should I optimize this database query?"
3. **Continue conversation** with follow-up questions
4. **Start fresh session** with `SPC a s` when changing topics

**Intelligent Refactoring:**
1. **Select code block** or entire function
2. **Press `SPC a r`** for refactoring suggestions
3. **Review suggestions** with detailed explanations and preservation guarantees

#### Model Recommendations

**Recommended Models (in order of preference):**

1. **`deepseek-r1:8b-0528-qwen3-q4_K_M`** (Default)
   - Best balance of performance and resource usage
   - Native thinking mode support
   - Excellent for coding tasks

2. **`cogito:latest`** (Cutting-edge)
   - Hybrid reasoning model
   - Outperforms DeepSeek on many benchmarks
   - Optimized for coding and STEM

3. **`deepseek-coder-v2:latest`** (Specialized)
   - GPT-4 Turbo level coding performance
   - Specialized for code generation
   - Mixture-of-Experts architecture

4. **`phi4:latest`** (Efficient)
   - Microsoft's latest 14B model
   - Good reasoning with lower resource requirements

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
    ├── ai-enhanced-coding.el # AI coding assistant (ENHANCED)
    ├── ai.el            # Minuet AI code completion
    ├── buffer-management.el # Buffer cleanup and organization
    ├── completion.el    # Completion frameworks (Enhanced)
    ├── dashboard-config.el # Startup dashboard
    ├── development.el   # Programming tools
    ├── dired-config.el  # File manager enhancements
    ├── docker-integration.el # Docker support
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

### AI Coding Assistant Configuration (Enhanced)
```elisp
;; Enhanced AI Coding Assistant Settings
(setq ai-coding-model "deepseek-r1:8b-0528-qwen3-q4_K_M")  ; Recommended model
(setq ai-coding-ollama-host "localhost:11434")

;; Temperature settings for different modes
(setq ai-coding-temperature 0.3)              ; Quick mode (fast, focused)
(setq ai-coding-thinking-temperature 0.6)     ; Thinking mode (more creative)

;; Thinking mode default (toggle with SPC a t)
(setq ai-coding-thinking-mode t)               ; Start with thinking enabled

;; Custom instructions for different contexts
(setq ai-coding-base-instructions 
  "You are a Senior Software Engineer. Always be highly technical, objective, and clear.
   Comment every function modification with reasoning. Don't break existing logic.")

;; Context-specific instructions
(add-to-list 'ai-coding-context-instructions
  '(debug . "Focus on identifying bugs and providing fix suggestions with explanations."))
```

### Alternative Models Configuration
```elisp
;; For different use cases:

;; High performance (if you have enough RAM):
(setq ai-coding-model "deepseek-r1:32b")

;; Specialized for coding:
(setq ai-coding-model "deepseek-coder-v2:latest")

;; Cutting-edge reasoning:
(setq ai-coding-model "cogito:latest")

;; Efficient for lower-end hardware:
(setq ai-coding-model "phi4:latest")
```

### Minuet AI Configuration (Legacy Multi-Provider)
```elisp
;; Minuet for AI code completion with cloud providers
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
5. **Theme Customization**: Modify `core/ui.el` to set  preferred default theme

### Custom Theme Setup

To set a default theme that loads automatically:

1. Edit `core/ui.el`
2. Uncomment  preferred theme line:
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
* **AI Response Optimization**: Unicode character cleaning for better display performance

## Troubleshooting

### Common Issues

1. **Missing Icons**: Run `M-x all-the-icons-install-fonts`
2. **Font Errors**: The configuration safely handles missing fonts
3. **Theme Not Loading**: Use `SPC t T` to select a theme interactively
4. **Package Errors**: Delete `~/.emacs.d/elpa` and restart Emacs to reinstall packages

### AI Issues (Enhanced)

#### "Thinking mode not working"
- **Check model support**: Ensure you're using a reasoning model (DeepSeek-R1, Cogito, etc.)
- **Verify Ollama version**: Update to v0.6.0+ for thinking API support
- **Test thinking mode**: `ollama run deepseek-r1:8b-0528-qwen3-q4_K_M --think "test"`

#### "Unicode characters displaying incorrectly"
- **Fixed automatically**: The enhanced assistant includes Unicode character cleaning
- **If issues persist**: Check font configuration and install comprehensive Unicode fonts

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
ollama list

# Pull recommended model
ollama pull deepseek-r1:8b-0528-qwen3-q4_K_M

# Alternative models:
ollama pull deepseek-coder-v2:latest
ollama pull cogito:latest
ollama pull phi4:latest
```

#### "Poor coding performance"
- **Switch to specialized model**: `deepseek-coder-v2:latest`
- **Enable thinking mode**: `SPC a t` for complex problems
- **Try larger model**: `deepseek-r1:32b` if you have sufficient RAM

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

* **MAJOR: Enhanced AI Coding Assistant**: Complete overhaul with native thinking mode support
* **NEW: DeepSeek-R1 Integration**: State-of-the-art reasoning model for superior coding assistance
* **NEW: Unicode Character Cleaning**: Automatic removal of problematic control characters (fixes display artifacts)
* **NEW: Thinking Mode Toggle**: Switch between quick responses and detailed reasoning chains
* **ENHANCED: Multi-Session Chat**: Persistent conversation support with session management
* **IMPROVED: Response Display**: Separate thinking process and final content sections
* **FIXED: Ollama API Integration**: Uses native thinking parameter instead of prompt-based control
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
- **DeepSeek** for the exceptional R1 reasoning models
- **Ollama** for local AI capabilities with thinking mode support
- **Docker** for containerization
- **Ripgrep** for fast searching
- **All contributors** who made this possible

---

**ProEmacs** - A modern, fast, and beautiful Emacs configuration for developers who want power without the complexity.
