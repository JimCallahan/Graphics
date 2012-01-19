#version 330
  
smooth in vec4 Color;  // RGBA interpolated vertex color 
 
out vec4 FragColor;    // Automatically bound to framebuffer
  
void main()
{
  FragColor = Color;
}
