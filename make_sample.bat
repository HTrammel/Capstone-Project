rem make_sample.bat
rem purpose: generate a sample file for testing

set src_dir=data\final\en_us
set maxline=1000

head -n %maxline% %src_dir%\en_US.blogs.txt > %src_dir%\samp_blog.txt
head -n %maxline% %src_dir%\en_US.news.txt > %src_dir%\samp_news.txt
head -n %maxline% %src_dir%\en_US.twitter.txt > %src_dir%\samp_twit.txt

cat  %src_dir%\samp_blog.txt %src_dir%\samp_news.txt  %src_dir%\samp_twit.txt > .\Data\all_samp.txt
