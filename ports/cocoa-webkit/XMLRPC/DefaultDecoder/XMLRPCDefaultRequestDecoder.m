//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCDefaultRequestDecoder.h"

@implementation XMLRPCDefaultRequestDecoder

- (id)init {
    if (self = [super init]) {
        method = [[NSString alloc] init];
        stack = [[NSMutableArray alloc] init];
        insertionStack = [[NSMutableArray alloc] init];

        elementType = XMLRPCElementTypeString;
        elementValue = [[NSMutableString alloc] init];
    }
    return self;
}

- (void)decodeWithData:(NSData*)data {
    parser = [[NSXMLParser alloc] initWithData:data];
    [parser setDelegate:self];
    [parser setShouldResolveExternalEntities:YES];
    [parser parse];
}

- (NSString*)method {
    return method;
}

- (NSArray*)parameters {
    return stack;
}

- (void)setElementType:(XMLRPCElementType)element {
    elementType = element;
}

- (XMLRPCElementType)elementType {
    return elementType;
}

- (void)parser:(NSXMLParser*)parser foundCharacters:(NSString*)string {
    if (!elementValue) {
        elementValue = [[NSMutableString alloc] initWithString:string];
    } else {
        [elementValue appendString:string];
    }
}

- (void)parser:(NSXMLParser*)parser
    didStartElement:(NSString*)element
       namespaceURI:(NSString*)namespaceURI
      qualifiedName:(NSString*)qualifiedName
         attributes:(NSDictionary*)attributeDict {
    if ([element isEqualToString:@"methodName"]) {
        [self setElementType:XMLRPCElementTypeMethodName];
    } else if ([element isEqualToString:@"array"]) {
        NSMutableArray* array = [[NSMutableArray alloc] init];
        [self addObject:array];
        [insertionStack addObject:array];
    } else if ([element isEqualToString:@"struct"]) {
        [self setElementType:XMLRPCElementTypeDictionary];
    } else if ([element isEqualToString:@"int"] || [element isEqualToString:@"i4"]) {
        [self setElementType:XMLRPCElementTypeInteger];
    } else if ([element isEqualToString:@"double"]) {
        [self setElementType:XMLRPCElementTypeDouble];
    } else if ([element isEqualToString:@"boolean"]) {
        [self setElementType:XMLRPCElementTypeBoolean];
    } else if ([element isEqualToString:@"string"]) {
        [self setElementType:XMLRPCElementTypeString];
    } else if ([element isEqualToString:@"dateTime.iso8601"]) {
        [self setElementType:XMLRPCElementTypeDate];
    } else if ([element isEqualToString:@"base64"]) {
        [self setElementType:XMLRPCElementTypeData];
    }
}

- (void)parser:(NSXMLParser*)parser
 didEndElement:(NSString*)elementName
  namespaceURI:(NSString*)namespaceURI
 qualifiedName:(NSString*)qualifiedName {
    if ([elementName isEqualToString:@"array"]) {
        [insertionStack removeLastObject];
    } else if (elementValue != nil) {
        NSString* tmpElement = [XMLRPCElementDecoder parseString:elementValue];
        elementValue = nil; // reset element value
        switch (elementType) {
            case XMLRPCElementTypeMethodName:
                method = tmpElement;
                break;
            case XMLRPCElementTypeString:
                [self addObject:tmpElement];
                break;
            case XMLRPCElementTypeInteger:
                [self addObject:[XMLRPCElementDecoder parseInteger:tmpElement]];
                break;
            case XMLRPCElementTypeDouble:
                [self addObject:[XMLRPCElementDecoder parseDouble:tmpElement]];
                break;
            case XMLRPCElementTypeBoolean:
                [self addObject:[XMLRPCElementDecoder parseBoolean:tmpElement]];
                break;
            case XMLRPCElementTypeDate:
                [self addObject:[XMLRPCElementDecoder parseDate:tmpElement]];
                break;
            case XMLRPCElementTypeData:
                [self addObject:[XMLRPCElementDecoder parseData:tmpElement]];
                break;
            default:
                break;
        }
    }
}

- (void)addObject:(id) object {
    if ([insertionStack count] > 0) {
        [[insertionStack lastObject] addObject:object];
    } else {
        [stack addObject:object];
    }
}

@end
