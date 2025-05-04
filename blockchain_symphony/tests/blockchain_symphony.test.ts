import { callReadOnlyFunction, makeContractCall, broadcastTransaction, standardPrincipalCV, uintCV, stringAsciiCV } from '@stacks/transactions'
import { StacksTestnet } from '@stacks/network'

const network = new StacksTestnet()
const contractAddress = 'STTESTADDRESS'
const contractName = 'blockchain-symphony'

export async function createNote(noteValue: number, octave: number, duration: number): Promise<number> {
  const tx = await makeContractCall({
    contractAddress,
    contractName,
    functionName: 'create-note',
    functionArgs: [uintCV(noteValue), uintCV(octave), uintCV(duration)],
    senderKey: 'YOUR_PRIVATE_KEY',
    network,
  })
  const result = await broadcastTransaction(tx, network)
  // Extract noteId from result
  return result.noteId ?? 1 // Simulated return
}

export async function createComposition(title: string, type: number): Promise<number> {
  const tx = await makeContractCall({
    contractAddress,
    contractName,
    functionName: 'create-composition',
    functionArgs: [stringAsciiCV(title), uintCV(type)],
    senderKey: 'YOUR_PRIVATE_KEY',
    network,
  })
  const result = await broadcastTransaction(tx, network)
  return result.compositionId ?? 1 // Simulated return
}

export async function getNote(noteId: number): Promise<any> {
  const response = await callReadOnlyFunction({
    contractAddress,
    contractName,
    functionName: 'get-note',
    functionArgs: [uintCV(noteId)],
    network,
    senderAddress: contractAddress,
  })
  return response.value?.data || {}
}
