import {
    Clarinet,
    Tx,
    Chain,
    Account,
    types
} from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Affiliate registration test",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const wallet1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('affiliate', 'register-affiliate', [], wallet1.address)
        ]);
        
        block.receipts[0].result.expectOk().expectBool(true);
    },
});

Clarinet.test({
    name: "Product listing and sale test",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const seller = accounts.get('wallet_1')!;
        const affiliate = accounts.get('wallet_2')!;
        const buyer = accounts.get('wallet_3')!;
        
        let block = chain.mineBlock([
            // Register affiliate
            Tx.contractCall('affiliate', 'register-affiliate', [], affiliate.address),
            
            // Add product
            Tx.contractCall('affiliate', 'add-product', [
                types.uint(1),
                types.uint(1000)
            ], seller.address),
            
            // Process sale
            Tx.contractCall('affiliate', 'process-sale', [
                types.uint(1),
                types.principal(affiliate.address)
            ], buyer.address)
        ]);
        
        block.receipts.forEach(receipt => {
            receipt.result.expectOk();
        });
    },
});

Clarinet.test({
    name: "Commission rate management test",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('affiliate', 'set-commission-rate', [
                types.uint(15)
            ], deployer.address)
        ]);
        
        block.receipts[0].result.expectOk().expectBool(true);
        
        let getRate = chain.mineBlock([
            Tx.contractCall('affiliate', 'get-commission-rate', [], deployer.address)
        ]);
        
        getRate.receipts[0].result.expectOk().expectUint(15);
    },
});
