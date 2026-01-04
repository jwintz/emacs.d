/**
 * Tecendil Wrapper - Non-invasive wrapper for upstream tecendil.mjs
 * 
 * This wrapper isolates the upstream tecendil.mjs from our code,
 * making it easy to update tecendil.mjs without breaking our implementation.
 */

// Re-export core functions with stable API
export { transcribe, loadMode, toFont } from './tecendil.mjs';
