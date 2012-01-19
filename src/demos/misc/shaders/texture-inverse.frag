#version 330
  
uniform sampler2D Texture;   // The texture sampler.

in block
{
  vec2 TexCoord;          // UV interpolated texture coordinates.
} In;

layout(location = 0, index = 0) out vec4 Color;              

void main()
{    
  Color = vec4(vec3(1.0, 1.0, 1.0) - texture(Texture, In.TexCoord).rgb, 1.0);
}
