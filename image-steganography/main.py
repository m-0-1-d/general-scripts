from PIL import Image
import string

SUPPORTED_CHARS = string.ascii_uppercase + string.digits + ' .'
END_MARKER = "DBP"

def encode_message(image_path, message, output_path):
    img = Image.open(image_path)
    img = img.convert("RGB")
    pixels = img.load()
    width, height = img.size
    
    message = ''.join(c for c in message if c in SUPPORTED_CHARS) + END_MARKER
    
    binary_message = ''.join(format(SUPPORTED_CHARS.index(c), '06b') for c in message)
    
    index = 0
    for y in range(height):
        for x in range(width):
            if index < len(binary_message):
                r, g, b = pixels[x, y][:3]
                new_r = (r & ~1) | int(binary_message[index])
                pixels[x, y] = (new_r, g, b)
                index += 1
            else:
                img.save(output_path)
                return
    
    img.save(output_path)

def decode_message(image_path):
    img = Image.open(image_path)
    img = img.convert("RGB")
    pixels = img.load()
    width, height = img.size
    
    binary_message = ""
    for y in range(height):
        for x in range(width):
            r, g, b = pixels[x, y][:3]
            binary_message += str(r & 1)
    
    decoded_text = ""
    for i in range(0, len(binary_message), 6):
        char_bin = binary_message[i:i+6]
        if len(char_bin) < 6:
            break
        char_index = int(char_bin, 2)
        if char_index < len(SUPPORTED_CHARS):
            decoded_text += SUPPORTED_CHARS[char_index]
        
        if decoded_text.endswith(END_MARKER):
            return decoded_text[:-len(END_MARKER)]
    
    return decoded_text

encode_message("input.png", "HELLO WORLD 123.", "output.png")
print(decode_message("output.png"))