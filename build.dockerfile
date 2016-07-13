FROM ubuntu
RUN sudo apt-get update
RUN sudo apt-get install wget -y
RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
RUN sudo apt-get update
RUN sudo apt-get install stack -y
RUN sudo apt-get install libgtk-3-dev -y
RUN sudo apt-get install build-essential -y
ADD . /app
WORKDIR /app
RUN make
