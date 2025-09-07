# Multi-stage build for Symbolic AI Knowledge Base System
FROM alpine:3.19 as builder

# Install build dependencies
RUN apk add --no-cache \
    emacs-nox \
    sqlite \
    sqlite-dev \
    curl \
    make \
    git

# Copy source code
WORKDIR /app
COPY *.el ./
COPY *.md ./
COPY .dockerignore ./

# Validate Emacs Lisp syntax
RUN emacs --batch --eval "(progn \
    (require 'bytecomp) \
    (setq byte-compile-error-on-warn t) \
    (byte-compile-file \"kb-advanced-system.el\") \
    (byte-compile-file \"kb-microtheories.el\") \
    (byte-compile-file \"kb-inference-engine.el\") \
    (byte-compile-file \"kb-nonmonotonic.el\") \
    (byte-compile-file \"kb-events.el\") \
    (byte-compile-file \"kb-debugger.el\") \
    (byte-compile-file \"kb-cache.el\") \
    (byte-compile-file \"kb-testing.el\") \
    (byte-compile-file \"kb-rdf.el\") \
    (byte-compile-file \"kb-database.el\") \
    (byte-compile-file \"kb-system.el\"))"

# Production stage
FROM alpine:3.19

# Install runtime dependencies
RUN apk add --no-cache \
    emacs-nox \
    sqlite \
    curl \
    bash \
    && addgroup -g 1001 kbuser \
    && adduser -D -u 1001 -G kbuser kbuser

# Create directories
RUN mkdir -p /app/data /app/logs \
    && chown -R kbuser:kbuser /app

# Copy application files
WORKDIR /app
COPY --from=builder --chown=kbuser:kbuser /app/*.el ./
COPY --from=builder --chown=kbuser:kbuser /app/*.md ./

# Copy byte-compiled files if they exist
COPY --from=builder --chown=kbuser:kbuser /app/*.elc ./ 2>/dev/null || true

# Create startup script
RUN cat > /app/start-kb.sh << 'EOF' && \
    chmod +x /app/start-kb.sh && \
    chown kbuser:kbuser /app/start-kb.sh
#!/bin/bash

echo "Starting Symbolic AI Knowledge Base System v2.1"
echo "=============================================="

# Initialize SQLite database
sqlite3 /app/data/kb.sqlite "CREATE TABLE IF NOT EXISTS version (version TEXT);"
sqlite3 /app/data/kb.sqlite "INSERT OR REPLACE INTO version VALUES ('2.1');"

# Start Emacs with the knowledge base system
exec emacs --batch --no-init-file --eval "
(progn
  (message \"Loading Knowledge Base System...\")
  (add-to-list 'load-path \"/app\")
  
  ;; Load core system
  (require 'kb-advanced-system)
  
  ;; Initialize the system
  (kb-init)
  
  ;; Connect to database
  (kb-db-connect \"/app/data/kb.sqlite\")
  
  ;; Enable caching for better performance
  (kb-cache-on 1000 300)
  
  ;; Run demonstration
  (message \"Running knowledge base demonstration...\")
  (kb-demo)
  
  ;; Show system status
  (kb-status)
  
  ;; Keep running for interactive use
  (message \"Knowledge Base System ready. Use C-c C-c to exit.\")
  (while t
    (sleep-for 1)))" "$@"
EOF

# Create interactive script
RUN cat > /app/interactive.sh << 'EOF' && \
    chmod +x /app/interactive.sh && \
    chown kbuser:kbuser /app/interactive.sh
#!/bin/bash

echo "Starting Interactive Knowledge Base Session"
echo "=========================================="

exec emacs -nw --eval "
(progn
  (add-to-list 'load-path \"/app\")
  (require 'kb-advanced-system)
  (kb-init)
  (kb-db-connect \"/app/data/kb.sqlite\")
  (kb-cache-on)
  (message \"Knowledge Base System loaded. Try: M-x kb-demo\"))"
EOF

# Switch to non-root user
USER kbuser

# Set environment variables
ENV KB_DATA_DIR=/app/data
ENV KB_LOG_DIR=/app/logs
ENV EMACS_SERVER_FILE=/app/data/.emacs-server

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD test -f /app/data/kb.sqlite || exit 1

# Default command
CMD ["/app/start-kb.sh"]

# Labels
LABEL maintainer="Knowledge Base System"
LABEL version="2.1"
LABEL description="Advanced Symbolic AI Knowledge Base System with Emacs Lisp"
LABEL org.opencontainers.image.source="https://github.com/awdemos/symbolic_ai_elisp_knowledge_base"
