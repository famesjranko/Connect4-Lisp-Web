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

# Create app directory
WORKDIR /app

# Install Quicklisp and manually create init file (avoids interactive prompt)
RUN curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(quit)' && \
    rm /tmp/quicklisp.lisp && \
    echo ';;; Quicklisp init' > /root/.sbclrc && \
    echo '#-quicklisp' >> /root/.sbclrc && \
    echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))' >> /root/.sbclrc && \
    echo '  (when (probe-file quicklisp-init) (load quicklisp-init)))' >> /root/.sbclrc

# Pre-load dependencies to cache them in the image
RUN sbcl --non-interactive \
         --eval '(ql:quickload (list :hunchentoot :cl-json))' \
         --eval '(quit)'

# Copy application files
COPY src/ ./src/
COPY static/ ./static/
COPY web-server.lisp ./

# Expose port
EXPOSE 8080

# Health check - check if server responds
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8080/ || exit 1

# Run the server
CMD ["sbcl", "--load", "web-server.lisp"]
