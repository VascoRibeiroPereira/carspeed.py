## To use this python program in RPI Zero install the OpenCV by this simple method:

```
echo 'deb [trusted=yes] http://dl.bintray.com/yoursunny/PiZero stretch-backports main' |\
  sudo tee /etc/apt/sources.list.d/bintray-yoursunny-PiZero.list
sudo apt update
sudo apt install python3-opencv
python3 -c 'import cv2; print(cv2.__version__)'
```
