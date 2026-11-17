# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Jekyll-based personal blog hosted on GitHub Pages. The site uses a custom "Field Notes" design (no Jekyll theme) and publishes programming tutorials, design patterns, and technical essays.

## Commands

### Local Development
```bash
# The Gemfile pins the github-pages gem, which needs Ruby 3.x (not Ruby 4).
# Homebrew: brew install ruby@3.3
export PATH="/opt/homebrew/opt/ruby@3.3/bin:$PATH"
bundle config set --local path vendor/bundle
bundle install

# Serve the site locally
bundle exec jekyll serve --livereload
```

### Building
```bash
# Build the site for production
bundle exec jekyll build
```

## Architecture

### Directory Structure
- `_posts/`: Markdown blog posts with YAML front matter
  - Posts follow naming convention: `YYYY-MM-DD-title.md`
  - Front matter includes: layout, title, date, tags, id, published
- `_layouts/`: Jekyll templates
  - `default.html`: Base layout with top nav, dark-mode toggle, footer
  - `post.html`: Blog post layout with date, reading time, tags, prev/next links
- `_includes/`: Footer SVGs (`moose.svg`, `pines.svg`)
- `assets/`: Static files (CSS, images)
  - `assets/css/style.scss`: All site styles (light/dark tokens, syntax highlighting, callouts)
  - `assets/js/site.js`: Dark-mode toggle
  - `assets/img/topo.svg`: Header topographic background
  - Post-specific images in subdirectories (e.g., `stmPost/`, `scottyPost/`)
- `exampleCode/`: Code examples referenced in blog posts (e.g., Haskell STM examples)

### Configuration
- `_config.yml`: Jekyll configuration
  - Site title: "jkeuhlen" 
  - No theme; plugins: jekyll-feed (RSS), jekyll-seo-tag
  - No analytics

### Content Structure
- Posts cover programming topics (Haskell, STM, web servers, etc.)
- Example code is stored in `exampleCode/` and referenced in posts
- Callouts: `<div class="callout callout-note|callout-warning|callout-danger">` with a `callout-title` paragraph
- Optional `description:` front matter overrides the home-page excerpt
- Navigation includes: Writing, Projects, Topics, About, Talks, Contact

### Hosting
- Deployed via GitHub Pages
- Custom domain: jkeuhlen.com
- Uses CNAME file for domain configuration