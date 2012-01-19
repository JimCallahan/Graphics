#version 330
 
layout(location = 0) in vec2 InPosition;  // 2D position of vertex
layout(location = 1) in vec3 InColor;     // RGB color of vertex
 
smooth out vec4 Color;                    // RGBA interpolated vertex color 
 
uniform mat4 MvpMatrix;
 
void main()
{
  gl_Position = MvpMatrix * vec4(InPosition, 0.0, 1.0);
  Color = vec4(InColor, 1.0);
}
