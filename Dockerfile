# Connect-4 Lisp AI with Web Interface
# Uses SBCL (Steel Bank Common Lisp) + Quicklisp + Hunchentoot

FROM debian:bookworm-slim

# Install SBCL and dependencies
RUN apt-get update && apt-get install -y \
    sbcl \
    curl \
    ca-certificates \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash lisp

# Create app directory with correct ownership
WORKDIR /app
RUN chown lisp:lisp /app

# Switch to non-root user for Quicklisp install
USER lisp

# Install Quicklisp
RUN curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(quit)' && \
    rm /tmp/quicklisp.lisp && \
    echo ';;; Quicklisp init' > ~/.sbclrc && \
    echo '#-quicklisp' >> ~/.sbclrc && \
    echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))' >> ~/.sbclrc && \
    echo '  (when (probe-file quicklisp-init) (load quicklisp-init)))' >> ~/.sbclrc

# Pre-load dependencies to cache them in the image
RUN sbcl --non-interactive \
         --eval '(ql:quickload (list :hunchentoot :cl-json :bordeaux-threads))' \
         --eval '(quit)'

# Copy application files (as root, then fix permissions)
USER root
COPY --chown=lisp:lisp src/ ./src/
COPY --chown=lisp:lisp static/ ./static/
COPY --chown=lisp:lisp web-server.lisp ./

# Switch back to non-root user
USER lisp

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8080/api/health || exit 1

# Run the server
CMD ["sbcl", "--load", "web-server.lisp"]
