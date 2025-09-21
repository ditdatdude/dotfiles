#!/bin/bash
# Setup script for AI interaction and denote directories

echo "Setting up AI interaction and denote directories..."

# Base directories
ORG_DIR="/mnt/c/Users/chip/Dropbox/org"
AI_SESSIONS_DIR="$ORG_DIR/ai-sessions"
DENOTE_DIR="$ORG_DIR/denote"
TEMP_AI_DIR="/tmp/ai-sessions"

# Create directories
echo "Creating directory structure..."
mkdir -p "$ORG_DIR"
mkdir -p "$AI_SESSIONS_DIR"
mkdir -p "$DENOTE_DIR"
mkdir -p "$TEMP_AI_DIR"

# Create initial denote configuration file
echo "Creating denote README..."
cat > "$DENOTE_DIR/README.org" << 'INNER_EOF'
#+TITLE: Denote Knowledge Base
#+DATE: 2025-01-22

* About This Directory

This directory contains notes managed by Denote, a note-taking system for Emacs.

** File Naming Convention
Files follow the pattern: DATE--TITLE__KEYWORDS.org

** Common Keywords
- ai :: AI-related content, prompts, interactions
- prompt :: Specific prompts for AI systems
- project :: Project-related notes
- meeting :: Meeting notes and minutes
- idea :: Ideas and brainstorming
- reference :: Reference material and documentation

** Usage
- Create new notes: SPC n n (in xah-fly command mode)
- Find notes: SPC n f
- Link notes: SPC n i

* Getting Started

1. Create your first note with SPC n n
2. Use meaningful titles and relevant keywords
3. Link related notes together
4. Review and organize regularly
INNER_EOF

# Create AI session template
echo "Creating AI session template..."
cat > "$AI_SESSIONS_DIR/template.org" << 'INNER_EOF'
#+TITLE: AI Session Template
#+FILETAGS: ai template

* Session Overview
** Goal
What do you want to achieve in this session?

** Context
What background information is relevant?

* Interactions

** Query 1
*** My Input


*** AI Response


*** Notes


* Summary
** Key Insights


** Action Items

INNER_EOF

# Create workflow guide
echo "Creating workflow guide..."
cat > "$ORG_DIR/ai-workflow-guide.org" << 'INNER_EOF'
#+TITLE: AI Workflow Guide
#+DATE: 2025-01-22

* Quick Start
1. Open file/code you want to discuss with AI
2. Select region or use entire buffer  
3. Use SPC a c to copy with context
4. Paste into your AI tool (Claude, ChatGPT, etc.)
5. Copy AI response
6. Use SPC a p to paste response back
7. Optionally save to AI session

* Key Bindings (command mode)
- SPC a c - Copy for AI interaction
- SPC a p - Paste AI response  
- SPC a n - Create new AI session
- SPC a l - List AI sessions
- SPC n n - Create new note
- SPC n f - Find note file
INNER_EOF

# Set permissions
chmod -R 755 "$ORG_DIR" 2>/dev/null || true
chmod 755 "$TEMP_AI_DIR" 2>/dev/null || true

# Create symlinks
if [ ! -L "$HOME/ai-sessions" ]; then
    ln -sf "$AI_SESSIONS_DIR" "$HOME/ai-sessions"
    echo "Created symlink: ~/ai-sessions"
fi

if [ ! -L "$HOME/notes" ]; then
    ln -sf "$DENOTE_DIR" "$HOME/notes"  
    echo "Created symlink: ~/notes"
fi

# Test clipboard
echo "Testing clipboard..."
if command -v clip.exe >/dev/null 2>&1; then
    echo "test" | clip.exe
    result=$(powershell.exe -command 'Get-Clipboard' 2>/dev/null | tr -d '\r')
    if [ "$result" = "test" ]; then
        echo "✓ Clipboard working"
    else
        echo "⚠ Clipboard may have issues"
    fi
else
    echo "⚠ clip.exe not found"
fi

echo ""
echo "Setup complete!"
echo "Directory structure created:"
echo "  📁 $ORG_DIR"
echo "  📁 $AI_SESSIONS_DIR" 
echo "  📁 $DENOTE_DIR"
echo "  📁 $TEMP_AI_DIR"
echo ""
echo "Next steps:"
echo "1. Restart Emacs or reload configuration"
echo "2. Try creating a note with: SPC n n"
echo "3. Create an AI session with: SPC a n"
