export const assetData = async (filename: string): Promise<string> => {
  const url = import.meta.resolve('../assets/' + filename);
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }
    return await response.text();
  } catch (error) {
    if (error instanceof Error) {
      throw new Error(`${error.message}: ${url}`);
    } else {
      throw new Error(String(error));
    }
  }
}
