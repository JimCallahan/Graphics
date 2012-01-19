#version 330
  
uniform sampler2D Texture;   // The texture sampler.

in block
{
  vec2 TexCoord;    // UV interpolated texture coordinates.
  vec4 BaseColor;   // Interpolated base color. 
} In;

layout(location = 0, index = 0) out vec4 Color;              

void main()
{    
  Color = In.BaseColor * texture(Texture, In.TexCoord);
}
