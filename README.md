# ProEmacs: A Modular, Performance-Focused Emacs Configuration

ProEmacs is a highly modular, performance-optimized Emacs configuration designed for modern development workflows. Built with a focus on speed, usability, and extensibility, it provides a comprehensive set of features while maintaining fast startup times and responsive editing.

## Features

### Core Features

* **Modular Architecture**: Cleanly separated configuration files for better organization and maintenance
* **Evil Mode Integration**: Vim keybindings with extensive customization and proper evil-collection setup
* **Performance Optimizations**: GC tuning, lazy loading, native compilation support
* **Modern UI**: Clean, distraction-free interface with Doom themes and modeline
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

* **Enhanced Dired**: Powerful file management with extensions
* **Buffer Management**: Intelligent buffer cleanup and organization
* **Window Management**: Flexible layouts with hydra-based controls
* **Tab System**: Workspace management with tab-bar-mode
* **Process Management**: Background process lifecycle optimization

### Themes & Appearance

* **Doom Themes**: Modern, beautiful theme collection with 15+ variants
* **Doom Modeline**: Informative, stylish modeline with icons
* **Theme Selector**: Easy switching between themes via `SPC t T`
* **Font Optimization**: Safe font loading with system compatibility checks

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

```bash
# Backup existing Emacs configuration
mv ~/.emacs.d ~/.emacs.d.bak

# Clone the repository
git clone https://github.com/sirjoaogoncalves/ProEmacs.git ~/.emacs.d

# Start Emacs
emacs
```

On first launch, the configuration will automatically install all required packages. This may take a few minutes.

### Post-Installation Setup

1. **Install Fonts** (optional, for better icon display):
   ```
   M-x all-the-icons-install-fonts
   ```

2. **Choose a Theme**:
   - Press `SPC t T` for quick doom theme selection
   - Or press `SPC t t` for all available themes

3. **Set Default Theme** (optional):
   Edit `core/ui.el` and uncomment your preferred theme:
   ```elisp
   (load-theme 'doom-one t)  ; Popular dark theme
   ```

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
* `SPC t` - Toggle options (including themes)
* `SPC s` - Search operations

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
    ├── ai.el            # AI code completion
    ├── buffer-management.el # Buffer cleanup and organization
    ├── completion.el    # Completion frameworks
    ├── dashboard-config.el # Startup dashboard
    ├── development.el   # Programming tools
    ├── dired-config.el  # File manager enhancements
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

## AI Integration

The configuration includes Minuet for AI code completion:

* `M-y` - Complete with minibuffer
* `M-i` - Show suggestion
* `C-c m` - Configure provider
* `SPC a m` - Show Minuet suggestion
* `SPC a c` - Configure Minuet

## Troubleshooting

### Common Issues

1. **Missing Icons**: Run `M-x all-the-icons-install-fonts`
2. **Font Errors**: The configuration safely handles missing fonts
3. **Theme Not Loading**: Use `SPC t T` to select a theme interactively
4. **Package Errors**: Delete `~/.emacs.d/elpa` and restart Emacs to reinstall packages

### Evil Collection Warnings

If you see warnings about `evil-want-keybinding`, the configuration has been fixed to properly set this variable before loading evil-collection.

### Performance Issues

* Use `SPC P` for performance tools and monitoring
* Check `SPC P t` for package load times
* Use `SPC P g` to manually trigger garbage collection

## Recent Changes

* **Fixed Evil Collection Integration**: Properly configured `evil-want-keybinding` to eliminate warnings
* **Added Doom Themes**: Complete doom-themes package with 15+ theme variants
* **Added Doom Modeline**: Modern, informative modeline with icons
* **Enhanced Theme System**: Quick theme selector accessible via `SPC t T`
* **Improved Package Management**: More robust package initialization and error handling
* **Removed Neotree**: Eliminated file explorer to resolve icon dependency issues
* **Enhanced Font Handling**: Safe font loading with system compatibility checks

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

---

**ProEmacs** - A modern, fast, and beautiful Emacs configuration for developers who want power without the complexity.
