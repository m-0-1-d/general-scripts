# image-steganography [In progress]

# LSB (Least Significant Bit)

In this method, the message is encoded in the least significant bits of each pixel in the image. The working principle:  

1. **Data Preparation:** The message is converted into a binary format.  
2. **Pixel Modification:** The last bit of the color channels (RGB) is replaced with a bit from the message.  
3. **Image Saving:** Visually, the image appears unchanged since the modifications are too small to notice.  

In our version, we will be able to encode A-Z, digits, spaces, and periods. The end-of-message marker will be the letter sequence "DBP". If the message is longer than the available bits, only a part of it will be encoded. Any unrecognized characters in the message will simply be ignored.
