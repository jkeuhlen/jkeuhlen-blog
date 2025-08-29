# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Jekyll-based personal blog hosted on GitHub Pages. The site uses the `jekyll-theme-architect` theme and publishes programming tutorials, design patterns, and technical essays.

## Commands

### Local Development
```bash
# Install Jekyll and dependencies
gem install jekyll bundler
bundle install

# Serve the site locally (if Gemfile exists)
bundle exec jekyll serve

# Alternative if no Gemfile
jekyll serve
```

### Building
```bash
# Build the site for production
jekyll build
```

## Architecture

### Directory Structure
- `_posts/`: Markdown blog posts with YAML front matter
  - Posts follow naming convention: `YYYY-MM-DD-title.md`
  - Front matter includes: layout, title, date, tags, id, published
- `_layouts/`: Jekyll templates
  - `default.html`: Base layout with navigation, Google Analytics
  - `post.html`: Blog post layout with date and metadata
- `assets/`: Static files (CSS, images)
  - `assets/css/custom.css`: Custom styles for callout boxes
  - Post-specific images in subdirectories (e.g., `stmPost/`)
- `exampleCode/`: Code examples referenced in blog posts (e.g., Haskell STM examples)

### Configuration
- `_config.yml`: Jekyll configuration
  - Site title: "jkeuhlen" 
  - Theme: jekyll-theme-architect
  - Google Analytics tracking enabled
  - Jekyll-feed plugin for RSS

### Content Structure
- Posts cover programming topics (Haskell, STM, web servers, etc.)
- Example code is stored in `exampleCode/` and referenced in posts
- Custom CSS provides styled callout boxes for notes/warnings
- Navigation includes: Home, About, Blog, Talks, Contact

### Hosting
- Deployed via GitHub Pages
- Custom domain: jkeuhlen.com
- Uses CNAME file for domain configuration