# ProEmacs: A Modular, Performance-Focused Emacs Configuration

ProEmacs is a highly modular, performance-optimized Emacs configuration designed for modern development workflows. Built with a focus on speed, usability, and extensibility, it provides a comprehensive set of features while maintaining fast startup times and responsive editing.

## Features

### Core Features

* **Modular Architecture**: Cleanly separated configuration files for better organization and maintenance
* **Evil Mode Integration**: Vim keybindings with extensive customization
* **Performance Optimizations**: GC tuning, lazy loading, native compilation support
* **Modern UI**: Clean, distraction-free interface with theme support
* **Keybinding System**: Intuitive, spacemacs-like keybindings with which-key integration

### Development Tools

* **LSP Support**: Integrated Language Server Protocol for intelligent code assistance
* **Syntax Checking**: On-the-fly error detection with flycheck
* **Version Control**: Comprehensive Git integration via Magit
* **Project Management**: Seamless project navigation with Projectile
* **Code Completion**: Fast, context-aware completion with Corfu and Cape
* **AI Assistance**: Integration with Minuet for AI code completion
* **Tree-sitter Support**: Enhanced syntax highlighting for Emacs 29+

### Organization & Navigation

* **File Explorer**: NERDTree-like file browsing with Neotree
* **Enhanced Dired**: Powerful file management with extensions
* **Buffer Management**: Intelligent buffer cleanup and organization
* **Window Management**: Flexible layouts with hydra-based controls
* **Tab System**: Workspace management with tab-bar-mode
* **Process Management**: Background process lifecycle optimization

### Writing & Documentation

* **Org Mode**: Complete Org setup with agenda, capture templates
* **Org Roam**: Knowledge management with bidirectional linking
* **Formatting Tools**: On-demand code and text formatting

## Installation

### Prerequisites

* Emacs 27.1+ (Emacs 29+ recommended for tree-sitter and native compilation)
* Git
* Optional: Language servers for development (LSP)

### Quick Install

```
bash# Backup existing Emacs configuration
mv ~/.emacs.d ~/.emacs.d.bak

# Clone the repository
git clone https://github.com/sirjoaogoncalves/ProEmacs.git ~/.emacs.d

# Start Emacs
emacs
```

On first launch, the configuration will automatically install all required packages.

## Key Bindings

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
* `SPC a` - AI tools
* `SPC t` - Toggle options
* `SPC s` - Search operations

Press `SPC` and wait for which-key to display available options.

## Project Structure

```
.
├── core
│   ├── defaults.el      # Better Emacs defaults
│   ├── keybindings.el   # Global key bindings
│   ├── packages.el      # Package management
│   └── ui.el            # User interface settings
├── early-init.el        # Early initialization (pre-GUI)
├── init.el              # Main initialization file
└── modules
    ├── ai.el            # AI code completion
    ├── buffer-management.el # Buffer cleanup and organization
    ├── completion.el    # Completion frameworks
    ├── dashboard-config.el # Startup dashboard
    ├── development.el   # Programming tools
    ├── dired-config.el  # File manager enhancements
    ├── evil-config.el   # Vim emulation
    ├── format-utils.el  # Code formatting
    ├── git.el           # Git integration
    ├── modern-features.el # Newer Emacs features
    ├── neotree-config.el # File explorer
    ├── org-config.el    # Org mode setup
    ├── performance.el   # Performance optimizations
    ├── process-manager.el # Background process management
    ├── remote-file-utils.el # Remote file operations
    ├── tabs.el          # Tab bar configuration
    ├── terminal.el      # Terminal integration
    ├── threading-utils.el # Thread pool management
    └── window-config.el # Window layout management
```

## Customization

ProEmacs is designed to be easily customizable:

1. **Personal Configuration**: Create a `personal.el` file in the root directory for personal settings
2. **Modifying Existing Modules**: Each module is self-contained and can be modified independently
3. **Adding New Modules**: Create new `.el` files in the `modules` directory and require them in `init.el`
4. **Custom Packages**: Add additional packages in `packages.el`

## Performance Features

ProEmacs includes several optimizations for a responsive editing experience:

* **Garbage Collection Strategy**: Intelligent GC adjustments during startup and idle time
* **Native Compilation**: Support for Emacs' native compilation (gccemacs)
* **Lazy Loading**: Deferred loading of non-essential packages
* **Process Management**: Automatic suspension of idle background processes
* **LSP Optimizations**: Tuned settings for responsive LSP operation
* **Thread Pool**: Background thread utilization for heavy operations (Emacs 28+)

## AI Integration

The configuration includes Minuet for AI code completion:

* `M-y` - Complete with minibuffer
* `M-i` - Show suggestion
* `C-c m` - Configure provider
* `SPC a m` - Show Minuet suggestion
* `SPC a c` - Configure Minuet

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
