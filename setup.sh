sudo redir :80 0.0.0.0:4000
cd app/
nohup R --no-save < app.R &
