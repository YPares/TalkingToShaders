module Graphics.Rendering.TalkingToShaders.Loading where

import Foreign
import Foreign.C
import Data.Functor
import Control.Monad

import Graphics.Rendering.OpenGL.Raw

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

checkStatus :: (Integral a1, Storable a1) =>
  GLenum
  -> (t -> GLenum -> Ptr a1 -> IO a)
  -> (t -> a1 -> Ptr a3 -> Ptr CChar -> IO a2)
  -> t
  -> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn id = do
    let fetch info = withNewPtr (glGetFn id info)
    status    <- toBool <$> fetch statusFlag
    logLength <- fetch gl_INFO_LOG_LENGTH
    when (logLength > 0) $
      allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
         glInfoLogFn id logLength nullPtr msgPtr
         when (not status) $
           peekCString msgPtr >>= error
    return status

loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderTypeFlag filePath = do
    code <- readFile filePath
    id   <- glCreateShader shaderTypeFlag
    withCString code $ \codePtr ->
      with codePtr $ \codePtrPtr ->
        glShaderSource id 1 codePtrPtr nullPtr
    glCompileShader id
    checkStatus gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog id
    return id

loadProgram :: [(FilePath, GLenum)]  -- ^ Paths with either gl_VERTEX_SHADER, gl_FRAGMENT_SHADER, etc.
            -> IO GLuint
loadProgram shadersAndTypes = do
    shaderIds <- mapM (uncurry $ flip loadShader) shadersAndTypes
    progId    <- glCreateProgram
    mapM_ (glAttachShader progId) shaderIds
    glLinkProgram progId
    checkStatus gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
    mapM_ glDeleteShader shaderIds
    return progId

