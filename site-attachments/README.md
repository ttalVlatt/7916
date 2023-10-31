# Development Notes

Push infinite hang after 
`Writing objects: 100% (10/10), 1.14 MiB | 760.00 KiB/s, done.`
`Total 10 (delta 3), reused 0 (delta 0), pack-reused 0`
issue resolved by increasing postBuffer size
`git config --global http.postBuffer 1048576000`
https://stackoverflow.com/questions/15843937/git-push-hangs-after-total-line